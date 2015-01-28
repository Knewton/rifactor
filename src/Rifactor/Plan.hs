{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

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
import           Control.Monad (foldM, forM)
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
            riEnvs <-
              initEnvs cfg lgr >>=
              runAWST dummyEnv . fetchFromAmazon
            case riEnvs of
              (Left err) -> print err >> exitFailure
              (Right riEnvs') ->
                do mapM_ (print)
                         (traverse (view modifications) riEnvs')

initEnvs :: Config -> Logger -> IO [RIEnv]
initEnvs cfg lgr =
  forM
    [(a,r) | r <- (cfg ^. regions)
           , a <- (cfg ^. accounts)]
    (\(a,r) ->
       riEnv <$>
       (getEnv r
               (FromKeys (AccessKey (B.pack (a ^. accessKey)))
                         (SecretKey (B.pack (a ^. secretKey)))) <&>
        (envLogger .~ lgr)))

{- TEST FNS -}

modifyResInstanceAsATest :: Env -> AWS ()
modifyResInstanceAsATest env' =
  runAWST env'
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
  return ()

printReservedInstanceModifications :: Env -> AWS ()
printReservedInstanceModifications env' =
  runAWST env'
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
  return ()

{- QUERY AMAZON -}

fetchFromAmazon :: [RIEnv] -> AWS [RIEnv]
fetchFromAmazon e = fetchPendingModifications e >>= fetchRunningInstances >>=
                    fetchActiveReservedInstances

fetchPendingModifications :: [RIEnv] -> AWS [RIEnv]
fetchPendingModifications =
  foldM (\a e ->
           do is <-
                hoistEither =<<
                runAWST (e ^. env)
                        (view drimrReservedInstancesModifications <$>
                         send (describeReservedInstancesModifications &
                               (drimFilters .~
                                [filter' "status" &
                                 fValues .~
                                 [T.pack "pending"]])))
              return ((e & modifications .~ is) :
                      a))
        []

fetchActiveReservedInstances :: [RIEnv] -> AWS [RIEnv]
fetchActiveReservedInstances =
  foldM (\a e ->
           do is <-
                hoistEither =<<
                runAWST (e ^. env)
                        (view drirReservedInstances <$>
                         send (describeReservedInstances & driFilters .~
                               [filter' "state" &
                                fValues .~
                                [toText RISActive]]))
              return ((e & reserved .~ is) :
                      a))
        []

fetchRunningInstances :: [RIEnv] -> AWS [RIEnv]
fetchRunningInstances =
  foldM (\a e ->
           do is <-
                hoistEither =<<
                runAWST (e ^. env)
                        (view dirReservations <$>
                         send (describeInstances & di1Filters .~
                               [filter' "instance-state-name" &
                                fValues .~
                                [toText ISNRunning]]))
              return ((e & instances .~
                       (concatMap (view rInstances) is)) :
                      a))
        []

{- INTERPRET DATA -}

interpret :: [RIEnv] -> [Resource]
interpret es =
  matchActiveReserved
    ([UnmatchedReserved (e ^. env)
                        r | e <- es
                          , r <- e ^. reserved] ++
     [UnmatchedInstance i | e <- es
                          , i <- e ^. instances])

matchActiveReserved :: [Resource] -> [Resource]
matchActiveReserved =
  match [] .
  partition isUnmatched
  where match ps ([],is) = ps ++ is
        match ps (rs,[]) = ps ++ rs
        match ps ((r@(UnmatchedReserved e ri):rs),is) =
          case (partition (reservedMatch r)
                          (is)) of
            ([],unmatched) ->
              match (r : ps)
                    (rs,unmatched)
            (matched,unmatched) ->
              let count =
                    fromMaybe 0 (ri ^. ri1InstanceCount)
                  (used,unused) =
                    splitAt count matched
                  lengthUsed = length used
                  ui =
                    (map (\(UnmatchedInstance i) -> i) used)
              in if lengthUsed == 0
                    then match (r : ps)
                               (rs,is)
                    else if lengthUsed == count
                            then match (UsedReserved e ri ui :
                                        ps)
                                       (rs,(unmatched ++ unused))
                            else match (PartialReserved e ri ui :
                                        ps)
                                       (rs,(unmatched ++ unused))
        match ps (rs,is) = ps ++ rs ++ is
        isUnmatched UnmatchedReserved{..} = True
        isUnmatched _ = False
        reservedMatch (UnmatchedReserved _ r) (UnmatchedInstance i) =
          (r ^. ri1InstanceType == i ^? i1InstanceType) &&
          (r ^. ri1AvailabilityZone == i ^. i1Placement ^. pAvailabilityZone)
        reservedMatch _ _ = False

matchPendingModifications :: [Resource] -> [Resource]
matchPendingModifications = id

{- MODIFICATIONS -}

matchPartialReserved :: [Resource] -> [Resource]
matchPartialReserved = id

matchUnusedReserved :: [Resource] -> [Resource]
matchUnusedReserved = id

{- RESIZING -}

combineUnusedReserved :: [Resource] -> [Resource]
combineUnusedReserved = id

resizeUnusedReserved :: [Resource] -> [Resource]
resizeUnusedReserved = id

{- DISPLAY -}

showMaybeText :: Maybe T.Text -> String
showMaybeText = T.unpack . fromMaybe (T.pack "n/a")

showMaybeNum :: Maybe Int -> String
showMaybeNum = show . fromMaybe 0

showMaybeInstanceType :: forall a. Show a => Maybe a -> [Char]
showMaybeInstanceType t =
  case t of
    Just t' -> map toLower (show t')
    Nothing -> "n/a"

showResource :: Resource -> String
showResource (UnmatchedReserved _ r) =
  showMaybeText (r ^. ri1AvailabilityZone) ++
  "," ++
  showMaybeInstanceType (r ^. ri1InstanceType) ++
  "," ++
  "reserved-instances (unmatched)" ++
  "," ++
  showMaybeText (r ^. ri1ReservedInstancesId) ++
  ",0," ++
  showMaybeNum (r ^. ri1InstanceCount)
showResource (PartialReserved _ r is) =
  showMaybeText (r ^. ri1AvailabilityZone) ++
  "," ++
  showMaybeInstanceType (r ^. ri1InstanceType) ++
  "," ++
  "reserved-instances (partial)" ++
  "," ++
  showMaybeText (r ^. ri1ReservedInstancesId) ++
  "," ++
  show (length is) ++
  "," ++
  showMaybeNum (r ^. ri1InstanceCount)
showResource (UsedReserved _ r is) =
  showMaybeText (r ^. ri1AvailabilityZone) ++
  "," ++
  showMaybeInstanceType (r ^. ri1InstanceType) ++
  "," ++
  "reserved-instances (used)" ++
  "," ++
  showMaybeText (r ^. ri1ReservedInstancesId) ++
  "," ++
  show (length is) ++
  "," ++
  showMaybeNum (r ^. ri1InstanceCount)
showResource (UnmatchedInstance i) =
  T.unpack (fromMaybe (T.pack "n/a")
                      (i ^. i1Placement ^. pAvailabilityZone)) ++
  "," ++
  map toLower (show (i ^. i1InstanceType)) ++
  "," ++
  "instance (unmatched)" ++
  "," ++
  T.unpack (i ^. i1InstanceId)
