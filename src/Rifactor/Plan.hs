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
            putStrLn (showPlan (interpret envs))

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
showPlan (Plan ps) = unlines (map showPlan ps)
showPlan (UnmatchedReserved _ r) =
  showMaybeText (r ^. ri1AvailabilityZone) ++
  "," ++
  showMaybeInstanceType (r ^. ri1InstanceType) ++
  "," ++
  showMaybeText (r ^. ri1ReservedInstancesId) ++
  ",0," ++
  showMaybeNum (r ^. ri1InstanceCount)
showPlan (PartialReserved _ r is) =
  showMaybeText (r ^. ri1AvailabilityZone) ++
  "," ++
  showMaybeInstanceType (r ^. ri1InstanceType) ++
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
  showMaybeText (r ^. ri1ReservedInstancesId) ++
  "," ++
  show (length is) ++
  "," ++
  showMaybeNum (r ^. ri1InstanceCount)
showPlan (UnmatchedInstance _ i) =
  T.unpack (fromMaybe (T.pack "n/a") (i ^. i1Placement ^. pAvailabilityZone)) ++
  "," ++
  show (i ^. i1InstanceType) ++
  "," ++
  T.unpack (i ^. i1InstanceId)

interpret :: [RIEnv] -> Plan
interpret es =
  let urs =
        [UnmatchedReserved (e ^. env)
                           r | e <- es
                             , r <- e ^. reserved]
      uis =
        [UnmatchedInstance (e ^. env)
                           i | e <- es
                             , i <- e ^. instances]
  in mkPlan urs uis []

mkPlan :: [Plan] -> [Plan] -> [Plan] -> Plan
mkPlan [] uis ps = Plan (ps ++ uis)
mkPlan urs [] ps = Plan (ps ++ urs)
mkPlan (ur@(UnmatchedReserved e r):urs) uis ps =
  case (partition (isMatch ur) uis) of
    ([],unmatched) ->
      mkPlan urs
             unmatched
             ((UnmatchedReserved e r) :
              ps)
    (matched,unmatched) ->
      let count =
            fromMaybe 0 (r ^. ri1InstanceCount)
          (used,unused) = splitAt count matched
          used' =
            map (\(UnmatchedInstance _ i) -> i) used
          lengthUsed = length used'
      in if lengthUsed == 0
            then mkPlan urs uis (ur : ps)
            else if lengthUsed == count
                    then mkPlan urs
                                (unmatched ++ unused)
                                (UsedReserved e r used' :
                                 ps)
                    else mkPlan urs
                                (unmatched ++ unused)
                                (PartialReserved e r used' :
                                 ps)
  where isMatch (UnmatchedReserved _ r') (UnmatchedInstance _ i) =
          (r' ^. ri1InstanceType == i ^? i1InstanceType) &&
          (r' ^. ri1AvailabilityZone == i ^. i1Placement ^. pAvailabilityZone)
        isMatch _ _ = False
mkPlan urs uis ps = Plan (ps ++ uis ++ urs)

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
