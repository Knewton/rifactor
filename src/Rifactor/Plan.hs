{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- Module      : Rifactor.Plan
-- Copyright   : (c) 2015 Knewton, Inc <se@knewton.com>
--               (c) 2015 Tim Dysinger <tim@dysinger.net> (contributor)
-- License     : Apache 2.0 http://opensource.org/licenses/Apache-2.0
-- Maintainer  : Tim Dysinger <tim@dysinger.net>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Rifactor.Plan where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.IO.Class ()
import           Control.Monad.Trans.AWS hiding (accessKey, secretKey)
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as B
import           Data.Char (toLower)
import           Data.Conduit (($$), ($=))
import qualified Data.Conduit.Attoparsec as C (sinkParser)
import qualified Data.Conduit.Binary as C (sourceFile)
import qualified Data.Conduit.List as C
import           Data.Foldable (traverse_)
import           Data.Traversable (for)
import           Data.List (partition)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Network.AWS.Data (toText)
import           Network.AWS.EC2
import           Rifactor.Types
import           System.Exit (exitFailure)
import           System.IO (stdout)

plan :: Options -> IO ()
plan opts =
  do config <-
       runResourceT $
       C.sourceFile (opts ^. file) $$
       C.sinkParser A.json
     case (A.fromJSON config :: A.Result Config) of
       (A.Error err) -> putStrLn err >> exitFailure
       (A.Success cfg) ->
         do lgr <-
              newLogger (if (opts ^. verbose)
                            then Trace
                            else Info)
                        stdout
            dummyEnv <-
              getEnv NorthVirginia
                     (FromKeys (AccessKey B.empty)
                               (SecretKey B.empty))
            es <- initEnvs cfg lgr
            pending <-
              runAWST dummyEnv (checkPendingModifications es)
            case pending of
              (Left err) -> print err >> exitFailure
              _ ->
                do results <-
                     runAWST dummyEnv (fetchFromAmazon es)
                   case results of
                     (Left err) -> print err >> exitFailure
                     (Right xs) ->
                       do let (reserved,nodes) = interpret xs
                          traverse_ (print . toCsvReserved) reserved
                          traverse_ (print . toCsvOnDemand) nodes

initEnvs :: Config -> Logger -> IO [Env]
initEnvs cfg lgr =
  for [(a,r) | r <- (cfg ^. regions)
             , a <- (cfg ^. accounts)]
      (\(a,r) ->
         (getEnv r
                 (FromKeys (AccessKey (B.pack (a ^. accessKey)))
                           (SecretKey (B.pack (a ^. secretKey)))) <&>
          (envLogger .~ lgr)))

checkPendingModifications :: [Env] -> AWS ()
checkPendingModifications =
  traverse_ (\e ->
               runAWST e
                       (do rims <-
                             view drimrReservedInstancesModifications <$>
                             send (describeReservedInstancesModifications &
                                   (drimFilters .~
                                    [filter' "status" &
                                     fValues .~
                                     [T.pack "processing"]]))
                           if null rims
                              then pure ()
                              else error "There are pending RI modifications."))

{- TEST FNS -}

modifyResInstanceAsATest :: Env -> AWS ()
modifyResInstanceAsATest e =
  runAWST e
          (send (modifyReservedInstances &
                 (mriReservedInstancesIds .~
                  [T.pack "130e039a-a4ed-48aa-8e7f-e48574098e22"]) &
                 (mriClientToken ?~ "ABC123") &
                 (mriTargetConfigurations .~
                  [(reservedInstancesConfiguration &
                    (ricAvailabilityZone ?~
                     T.pack "us-east-1c") &
                    (ricInstanceType ?~ M2_4XLarge) &
                    (ricPlatform ?~ "EC2-Classic") &
                    (ricInstanceCount ?~ 2))
                  ,(reservedInstancesConfiguration &
                    (ricAvailabilityZone ?~
                     T.pack "us-east-1b") &
                    (ricInstanceType ?~ M2_4XLarge) &
                    (ricPlatform ?~ "EC2-Classic") &
                    (ricInstanceCount ?~ 2))]))) >>=
  hoistEither >>
  pure ()

printReservedInstanceModifications :: Env -> AWS ()
printReservedInstanceModifications e =
  runAWST e
          (paginate describeReservedInstancesModifications $=
           (C.concatMap (view drimrReservedInstancesModifications)) $$
           (C.mapM_ (\rim ->
                       (info . T.concat)
                         ([T.pack (case (rim ^. rimCreateDate) of
                                     Just t -> show t
                                     Nothing -> "n/a")
                          ,T.pack ","
                          ,(fromMaybe T.empty (rim ^. rimStatus))
                          ,T.pack ","
                          ,(fromMaybe T.empty (rim ^. rimStatusMessage))
                          ,T.pack ","
                          ,fromMaybe T.empty
                                     (rim ^. rimReservedInstancesModificationId)
                          ,T.pack ","
                          ,T.intercalate
                             ","
                             (map (fromMaybe T.empty)
                                  (rim ^. rimReservedInstancesIds ^.. traverse .
                                   riiReservedInstancesId))])))) >>=
  hoistEither >>
  pure ()

{- QUERY AMAZON -}

fetchFromAmazon :: [Env] -> AWS ([Reserved],[OnDemand])
fetchFromAmazon es =
  pure (,) <*> fetchActiveReservedInstances es <*> fetchRunningInstances es

fetchActiveReservedInstances :: [Env] -> AWS [Reserved]
fetchActiveReservedInstances =
  liftA concat .
  traverse (\e ->
              do xs <-
                   hoistEither =<<
                   runAWST e
                           (view drirReservedInstances <$>
                            send (describeReservedInstances & driFilters .~
                                  [filter' "state" &
                                   fValues .~
                                   [toText RISActive]]))
                 pure (map (UnmatchedReserved e) xs))

fetchRunningInstances :: [Env] -> AWS [OnDemand]
fetchRunningInstances =
  liftA concat .
  traverse (\e ->
              do xs <-
                   hoistEither =<<
                   runAWST e
                           (view dirReservations <$>
                            send (describeInstances & di1Filters .~
                                  [filter' "instance-state-name" &
                                   fValues .~
                                   [toText ISNRunning]]))
                 pure (map OnDemand (concatMap (view rInstances) xs)))

{- INTERPRET DATA -}

interpret :: ([Reserved],[OnDemand])
          -> ([Reserved],[OnDemand])
interpret = matchReserved

matchUnmatchedReserved :: (Reserved -> OnDemand -> Bool)
                       -> ([Reserved],[OnDemand])
                       -> ([Reserved],[OnDemand])
matchUnmatchedReserved isMatchingInstances (reserved,nodes) =
  let (unmatchedReserved,otherReserved) =
        partition isUnmatchedReserved reserved
  in match otherReserved (unmatchedReserved,nodes)
  where match rs ([],ys) = (rs,ys)
        match rs (xs,[]) = (rs ++ xs,[])
        match rs ((x:xs),ys) =
          case (partition (isMatchingInstances x) ys) of
            ([],unmatched) ->
              match (x : rs)
                    (xs,unmatched)
            (matched,unmatched) ->
              let count =
                    fromMaybe 0 (x ^?! reReservedInstances ^. ri1InstanceCount)
                  (used,unused) =
                    splitAt count matched
                  lengthUsed = length used
                  uis =
                    map (\(OnDemand i) -> i) used
              in if lengthUsed == 0
                    then match (x : rs)
                               (xs,ys)
                    else if lengthUsed == count
                            then match (UsedReserved (x ^. reEnv)
                                                     (x ^?! reReservedInstances)
                                                     uis :
                                        rs)
                                       (xs,(unmatched ++ unused))
                            else match (PartialReserved
                                          (x ^. reEnv)
                                          (x ^?! reReservedInstances)
                                          uis :
                                        rs)
                                       (xs,(unmatched ++ unused))
        isUnmatchedReserved UnmatchedReserved{..} = True
        isUnmatchedReserved _ = False

matchReserved :: ([Reserved],[OnDemand]) -> ([Reserved],[OnDemand])
matchReserved = matchUnmatchedReserved isWorkableInstanceMatch .
                matchUnmatchedReserved isPerfectInstanceMatch -- TODO ADD CONSTRUCTORS FOR DIFFERENT INSTANCES
  where isPerfectInstanceMatch (UnmatchedReserved _ r) (OnDemand i) =
          (r ^. ri1AvailabilityZone == i ^. i1Placement ^. pAvailabilityZone) &&
          (r ^. ri1InstanceType == i ^? i1InstanceType)
        -- TODO Add network type (Classic vs VPN)
        isPerfectInstanceMatch _ _ = False
        isWorkableInstanceMatch (UnmatchedReserved _ r) (OnDemand i) =
          (r ^. ri1InstanceType == i ^? i1InstanceType)
        isWorkableInstanceMatch _ _ = False

{- DISPLAY -}

toCsvMaybeText :: Maybe T.Text -> String
toCsvMaybeText = T.unpack . fromMaybe (T.pack "n/a")

toCsvMaybeNum :: Maybe Int -> String
toCsvMaybeNum = show . fromMaybe 0

toCsvMaybeInstanceType :: forall a. Show a => Maybe a -> String
toCsvMaybeInstanceType t =
  case t of
    Just t' -> map toLower (show t')
    Nothing -> "n/a"

toCsvReserved :: Reserved -> String
toCsvReserved (UnmatchedReserved _ r) =
  toCsvMaybeText (r ^. ri1AvailabilityZone) ++
  "," ++
  toCsvMaybeInstanceType (r ^. ri1InstanceType) ++
  "," ++
  "reserved-instances (unmatched)" ++
  "," ++
  toCsvMaybeText (r ^. ri1ReservedInstancesId) ++
  ",0," ++
  toCsvMaybeNum (r ^. ri1InstanceCount)
toCsvReserved (PartialReserved _ r is) =
  toCsvMaybeText (r ^. ri1AvailabilityZone) ++
  "," ++
  toCsvMaybeInstanceType (r ^. ri1InstanceType) ++
  "," ++
  "reserved-instances (partial)" ++
  "," ++
  toCsvMaybeText (r ^. ri1ReservedInstancesId) ++
  "," ++
  show (length is) ++
  "," ++
  toCsvMaybeNum (r ^. ri1InstanceCount)
toCsvReserved (UsedReserved _ r is) =
  toCsvMaybeText (r ^. ri1AvailabilityZone) ++
  "," ++
  toCsvMaybeInstanceType (r ^. ri1InstanceType) ++
  "," ++
  "reserved-instances (used)" ++
  "," ++
  toCsvMaybeText (r ^. ri1ReservedInstancesId) ++
  "," ++
  show (length is) ++
  "," ++
  toCsvMaybeNum (r ^. ri1InstanceCount)
toCsvReserved _ = error "TODO"

toCsvOnDemand :: OnDemand -> String
toCsvOnDemand (OnDemand i) =
  T.unpack (fromMaybe (T.pack "n/a")
                      (i ^. i1Placement ^. pAvailabilityZone)) ++
  "," ++
  map toLower (show (i ^. i1InstanceType)) ++
  "," ++
  "instance (unmatched)" ++
  "," ++
  T.unpack (i ^. i1InstanceId)
