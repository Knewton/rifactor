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
import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.Trans.AWS hiding (accessKey, secretKey)
import           Control.Monad.Trans.Resource
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as B
import           Data.Conduit
import qualified Data.Conduit.Attoparsec as C
import qualified Data.Conduit.Binary as C
import           Data.List (groupBy, sortBy)
import           Data.Maybe (fromJust, mapMaybe)
import           Data.Monoid ((<>))
import           Data.Ord (comparing)
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
         newLogger Info stdout >>=
         initEnvs cfg >>=
         fetchActiveReservedInstances >>=
         fetchRunningInstances >>=
         printPlan

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
              -- TODO maybe we don't interpret the errs here during fetch?
              case is of
                Left err -> print err >> return a
                Right xs ->
                  return ((e & instances .~
                           concatMap (view rInstances) xs) :
                          a))
        []

printPlan :: [RIEnv] -> IO ()
printPlan es =
  do printReservedInstances es
     printRunningInstances es

printReservedInstances :: [RIEnv] -> IO ()
printReservedInstances es =
  forM_ (groupBy similarReservedInstances
                 (sortBy comparingReservedInstances (concatMap (view reserved) es)))
        (\g@(r:_) ->
           putStrLn (T.unpack (fromJust (r ^. ri1AvailabilityZone)) ++
                     "," ++
                     show (fromJust (r ^. ri1InstanceType)) ++
                     ",reserved," ++
                     show (foldl (+) 0 (mapMaybe (view ri1InstanceCount) g))))

printRunningInstances :: [RIEnv] -> IO ()
printRunningInstances es =
  forM_ (groupBy similarInstances (sortBy comparingInstances (concatMap (view instances) es)))
        (\g@(i:_) ->
           putStrLn (T.unpack (fromJust (i ^. i1Placement ^. pAvailabilityZone)) ++
                     "," ++
                     show (i ^. i1InstanceType) ++
                     ",instance," ++
                     show (length g)))

similarReservedInstances :: ReservedInstances -> ReservedInstances -> Bool
similarReservedInstances ri0 ri1 =
  (ri0 ^. ri1InstanceType == ri1 ^. ri1InstanceType) &&
  (ri0 ^. ri1AvailabilityZone == ri1 ^. ri1AvailabilityZone)

similarInstances :: Instance -> Instance -> Bool
similarInstances i0 i1 =
  (i0 ^. i1InstanceType == i1 ^. i1InstanceType) &&
  ((i0 ^. i1Placement ^. pAvailabilityZone) ==
   (i1 ^. i1Placement ^. pAvailabilityZone))

comparingReservedInstances :: ReservedInstances -> ReservedInstances -> Ordering
comparingReservedInstances a b =
  comparing (view ri1InstanceType) a b <>
  comparing (view ri1AvailabilityZone) a b

comparingInstances :: Instance -> Instance -> Ordering
comparingInstances a b =
  comparing (view i1InstanceType) a b <>
  comparing (view pAvailabilityZone . view i1Placement) a b

mapEnvM :: forall e a b.
           Show e
        => (b -> IO (Either e [a])) -> [b] -> IO [a]
mapEnvM f envs =
  foldM (\acc env' ->
           do e <- f env'
              case e of
                (Left err) ->
                  do print err
                     return acc
                (Right xs) -> return (acc ++ xs))
        []
        envs

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
