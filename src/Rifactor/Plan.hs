{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

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
import           Control.Monad
import           Control.Monad.Trans.AWS hiding (accessKey, secretKey)
import           Control.Monad.Trans.Resource
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as B
import           Data.Conduit
import           Data.Char (toLower)
import qualified Data.Conduit.Attoparsec as C
import qualified Data.Conduit.Binary as C
import           Data.List (partition)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Network.AWS.Data (toText)
import           Network.AWS.EC2
import           Rifactor.Types
import           System.IO

plan :: Options -> IO ()
plan opts =
  do config <- runResourceT $
               C.sourceFile (opts ^. configFile) $$
               C.sinkParser A.json
     case (A.fromJSON config :: A.Result Config) of
       (A.Error err) ->
         putStrLn ("Config File Error: " ++ err)
       (A.Success cfg) ->
         do envs <- fetchRunningInstances =<<
                    fetchActiveReservedInstances =<<
                    initEnvs cfg =<<
                    newLogger Info stdout
            putStrLn (unlines (map showPlan (interpret envs)))

initEnvs :: Config -> Logger -> IO [RIEnv]
initEnvs cfg lgr =
  -- TODO try something lensy like 'over traversal f'
  forM
    [(a,r) | r <- (cfg ^. regions)
           , a <- (cfg ^. accounts)]
    (\(a,r) ->
       riEnv <$>
       (getEnv r
               (FromKeys (AccessKey (B.pack (a ^. accessKey)))
                         (SecretKey (B.pack (a ^. secretKey)))) <&>
        (envLogger .~ lgr)))

fetchActiveReservedInstances :: [RIEnv] -> IO [RIEnv]
fetchActiveReservedInstances =
  foldM (\a e ->
           do is <- activeReservedInstances e
              -- TODO maybe we don't want lossy errs here during fetch? y mv to types
              case is of
                Left err -> print err >> return a
                Right xs ->
                  return ((e & reserved .~ xs) :
                          a))
        []

fetchRunningInstances :: [RIEnv] -> IO [RIEnv]
fetchRunningInstances =
  foldM (\a e ->
           do is <- runningInstances e
              -- TODO maybe we don't want lossy errs here during fetch? y mv to types
              case is of
                Left err -> print err >> return a
                Right xs ->
                  return ((e & instances .~
                           concatMap (view rInstances) xs) :
                          a))
        []

interpret :: [RIEnv] -> [Plan]
interpret es =
  let rs =
        [((e ^. env),r) | e <- es
                        , r <- e ^. reserved]
      is =
        [i | e <- es
           , i <- e ^. instances]
  in mkPlan rs is []

mkPlan :: [(Env, ReservedInstances)] -> [Instance] -> [Plan] -> [Plan]
mkPlan [] is ps =
  (ps ++
        map UnmatchedInstance is)
mkPlan rs [] ps =
  (ps ++
        map (\r ->
               UnmatchedReserved (r ^. _1)
                                 (r ^. _2))
            rs)
mkPlan (r:rs) is ps =
  case (partition (isMatch r)
                  (is)) of
    ([],unmatched) ->
      mkPlan rs
             unmatched
             (UnmatchedReserved (r ^. _1)
                                (r ^. _2) :
              ps)
    (matched,unmatched) ->
      let count =
            fromMaybe 0 (r ^. _2 ^. ri1InstanceCount)
          (used,unused) = splitAt count matched
          lengthUsed = length used
      in if lengthUsed == 0
            then mkPlan rs
                        is
                        (UnmatchedReserved (r ^. _1)
                                           (r ^. _2) :
                         ps)
            else if lengthUsed == count
                    then mkPlan rs
                                (unmatched ++ unused)
                                (UsedReserved (r ^. _1)
                                              (r ^. _2)
                                              used :
                                 ps)
                    else mkPlan rs
                                (unmatched ++ unused)
                                (PartialReserved (r ^. _1)
                                                 (r ^. _2)
                                                 used :
                                 ps)
  where isMatch r' i =
          (r' ^. _2 ^. ri1InstanceType == i ^? i1InstanceType) &&
          (r' ^. _2 ^. ri1AvailabilityZone == i ^. i1Placement ^.
                                              pAvailabilityZone)

runningInstances :: RIEnv -> IO (Either Error [Reservation])
runningInstances =
  flip runAWST
       (view dirReservations <$>
        send (describeInstances & di1Filters .~
              [filter' "instance-state-name" &
               fValues .~
               [toText ISNRunning]])) .
  view env

activeReservedInstances :: RIEnv -> IO (Either Error [ReservedInstances])
activeReservedInstances =
  flip runAWST
       (view drirReservedInstances <$>
        send (describeReservedInstances & driFilters .~
              [filter' "state" &
               fValues .~
               [toText RISActive]])) .
  view env

reservedInstancesModifications :: RIEnv
                               -> IO (Either Error [ReservedInstancesModification])
reservedInstancesModifications =
  flip runAWST
       (view drimrReservedInstancesModifications <$>
        send describeReservedInstancesModifications) .
  view env

showMaybeText :: Maybe T.Text -> String
showMaybeText = T.unpack . fromMaybe (T.pack "n/a")

showMaybeNum :: Maybe Int -> String
showMaybeNum = show . fromMaybe 0

showMaybeInstanceType :: forall a. Show a => Maybe a -> [Char]
showMaybeInstanceType t =
  case t of
    Just t' -> map toLower (show t')
    Nothing -> "n/a"

showPlan :: Plan -> String
showPlan (UnmatchedReserved _ r) =
  showMaybeText (r ^. ri1AvailabilityZone) ++
  "," ++
  showMaybeInstanceType (r ^. ri1InstanceType) ++
  "," ++
  "reserved-instances (unmatched)" ++
  "," ++
  showMaybeText (r ^. ri1ReservedInstancesId) ++
  ",0," ++
  showMaybeNum (r ^. ri1InstanceCount)
showPlan (PartialReserved _ r is) =
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
showPlan (UsedReserved _ r is) =
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
showPlan (UnmatchedInstance i) =
  T.unpack (fromMaybe (T.pack "n/a") (i ^. i1Placement ^. pAvailabilityZone)) ++
  "," ++
  map toLower (show (i ^. i1InstanceType)) ++
  "," ++
  "instance (unmatched)" ++
  "," ++
  T.unpack (i ^. i1InstanceId)
