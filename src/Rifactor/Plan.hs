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

import           Data.Conduit
import qualified Data.Conduit.List       as Conduit
import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS
import qualified Data.ByteString.Builder as Build
import           Data.Monoid
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Time.Clock.POSIX
import           Network.AWS.EC2
import           Rifactor.Types
import           System.IO

plan :: Options -> IO (Either Error ())
plan _opts =
  do lgr <- newLogger Info stdout
     env <- getEnv NorthVirginia Discover <&>
            envLogger .~
            lgr
     ts <- Text.pack . show <$> getTimestamp
     runAWST env $
       do say "Instances " ts
          result <- sendCatch describeInstances
          case result of
            (Left err) -> say "Error " err
            (Right response) ->
              do forM_ (view dirReservations response) $
                   \x ->
                     say "\t" (x ^. rReservationId)
          say "ReservedInstances " ts
          result0 <- sendCatch describeReservedInstances
          case result0 of
            (Left err) -> say "Error " err
            (Right response) ->
              do forM_ (view drirReservedInstances response) $
                   \x ->
                     say "\t" (x ^. ri1ReservedInstancesId)
          say "Volumes " ts
          result1 <- sendCatch describeVolumes
          case result1 of
            (Left err) -> say "Error " err
            (Right response) ->
              do forM_ (view dvrVolumes response) $
                   \x -> say "\t" (x ^. vVolumeId)
          say "Snapshots " ts
          snapshotsResult <- sendCatch describeSnapshots
          case snapshotsResult of
            (Left err) -> say "Error " err
            (Right response) ->
              do forM_ (view dsrSnapshots response) $
                   \x ->
                     say "\t" (x ^. sSnapshotId)
          say "Images " ts
          imagesResult <- sendCatch describeImages
          case imagesResult of
            (Left err) -> say "Error " err
            (Right response) ->
              do forM_ (view dirImages response) $
                   \x -> say "\t" (x ^. iImageId)
          say "Regions " ts
          regionsResult <- sendCatch describeRegions
          case regionsResult of
            (Left err) -> say "Error " err
            (Right response) ->
              do forM_ (view drrRegions response) $
                   \x ->
                     say "\t" (x ^. rRegionName)
          say "Addresses " ts
          addressesResult <- sendCatch describeAddresses
          case addressesResult of
            (Left err) -> say "Error " err
            (Right response) ->
              do forM_ (view darAddresses response) $
                   \x -> say "\t" (x ^. aPublicIp)
          say "Subnets " ts
          subnetsResult <- sendCatch describeSubnets
          case subnetsResult of
            (Left err) -> say "Error " err
            (Right response) ->
              do forM_ (view dsrSubnets response) $
                   \x ->
                     say "\t" (x ^. s1SubnetId)
          say "Key Pairs " ts
          keyPairsResult <- sendCatch describeKeyPairs
          case keyPairsResult of
            (Left err) -> say "Error " err
            (Right response) ->
              do forM_ (view dkprKeyPairs response) $
                   \x ->
                     say "\t" (x ^. kpiKeyName)
          say "VPCs " ts
          vpcsResult <- sendCatch describeVpcs
          case vpcsResult of
            (Left err) -> say "Error " err
            (Right response) ->
              do forM_ (view dvrVpcs response) $
                   \x -> say "\t" (x ^. vpcVpcId)

say :: Show a => Build.Builder -> a -> AWST IO ()
say msg = info . mappend msg . Build.stringUtf8 . show

getTimestamp :: IO Integer
getTimestamp = truncate <$> getPOSIXTime
