{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- Module      : Rifactor.AWS
-- Copyright   : (c) 2015 Knewton, Inc <se@knewton.com>
--               (c) 2015 Tim Dysinger <tim@dysinger.net> (contributor)
-- License     : Apache 2.0 http://opensource.org/licenses/Apache-2.0
-- Maintainer  : Tim Dysinger <tim@dysinger.net>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Rifactor.AWS where

import           BasePrelude hiding (getEnv)
import           Control.Lens
import           Control.Monad.IO.Class ()
import           Control.Monad.Trans.AWS hiding (accessKey, secretKey)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import           Network.AWS.Data (toText)
import           Network.AWS.EC2
import           Rifactor.Types

-- | We sometimes need a set of empty keys in a prepackaged Env so
-- that we can use the AWS transformer.  runAWST takes an Env and
-- there's no alternative except real environments at the moment.  So
-- we'll just hand it an Env with empty keys.  That way we can use the
-- AWS monad without fear of it doing things to our account(s).
noKeysEnv :: IO Env
noKeysEnv =
  getEnv NorthVirginia
         (FromKeys (AccessKey B.empty)
                   (SecretKey B.empty))

-- | Given our Config (was JSON), and a AWS Logger, create all the Env
-- records we need (1 for each region/account combo).  This is so we
-- can cycle through them when aggergating data on AWS EC2.
initEnvs :: Config -> Logger -> IO [Env]
initEnvs (Config accounts regions) lgr =
  for [(a,r) | r <- regions
             , a <- accounts]
      (\((Account _name key secret),r) ->
         (getEnv r
                 (FromKeys (AccessKey (B.pack key))
                           (SecretKey (B.pack secret))) <&>
          (envLogger .~ lgr)))

-- | Query for pending ReservedInstancesModifications.  We care about
-- this because we don't want to do anything else until these are no
-- longer pending.  If we do have a pending, just blow out with an
-- error & stops the AWS monad from doing anything further.
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

-- | Fetch all the Model data we need from Amazon via their APIs.
fetchFromAmazon :: [Env] -> AWS Model
fetchFromAmazon es =
  Model <$> fetchInstances es <*> fetchReservedInstances es <*> pure []

-- | Fetch all the ReservedInstances records we can from all Env (all
-- accounts/regions). Return a consolidated list of Reserved records
-- (1 per EC2 ReservedInstances).
fetchReservedInstances :: [Env] -> AWS [Reserved]
fetchReservedInstances =
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
                 pure (map (\r -> Reserved e r [] []) xs))

-- | Fetch all the Instance records we can from all AWS Env (all
-- accounts/regions). Return a consolidated list of OnDemand records
-- (1 per EC2 Instances).
fetchInstances :: [Env] -> AWS [OnDemand]
fetchInstances =
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
                 pure (map (OnDemand e) (concatMap (view rInstances) xs)))


instanceClass :: forall t.
                  Fractional t
               => InstanceType -> (InstanceGroup,t)
instanceClass C1_Medium = (C1,sizeFactor Medium)
instanceClass C1_XLarge = (C1,sizeFactor XLarge)
instanceClass C3_2XLarge = (C3,sizeFactor XLarge2X)
instanceClass C3_4XLarge = (C3,sizeFactor XLarge4X)
instanceClass C3_8XLarge = (C3,sizeFactor XLarge8X)
instanceClass C3_Large = (C3,sizeFactor Large)
instanceClass C3_XLarge = (C3,sizeFactor XLarge)
instanceClass C4_2XLarge = (C4,sizeFactor XLarge2X)
instanceClass C4_4XLarge = (C4,sizeFactor XLarge4X)
instanceClass C4_8XLarge = (C4,sizeFactor XLarge8X)
instanceClass C4_Large = (C4,sizeFactor Large)
instanceClass C4_XLarge = (C4,sizeFactor XLarge)
instanceClass CC1_4XLarge = (CC1,sizeFactor XLarge4X)
instanceClass CC2_8XLarge = (CC2,sizeFactor XLarge8X)
instanceClass CG1_4XLarge = (CG1,sizeFactor XLarge4X)
instanceClass CR1_8XLarge = (CR1,sizeFactor XLarge8X)
instanceClass G2_2XLarge = (G2,sizeFactor XLarge2X)
instanceClass HI1_4XLarge = (HI1,sizeFactor XLarge4X)
instanceClass HS1_8XLarge = (HS1,sizeFactor XLarge8X)
instanceClass I2_2XLarge = (I2,sizeFactor XLarge2X)
instanceClass I2_4XLarge = (I2,sizeFactor XLarge4X)
instanceClass I2_8XLarge = (I2,sizeFactor XLarge8X)
instanceClass I2_XLarge = (I2,sizeFactor XLarge)
instanceClass M1_Large = (M1,sizeFactor Large)
instanceClass M1_Medium = (M1,sizeFactor Medium)
instanceClass M1_Small = (M1,sizeFactor Small)
instanceClass M1_XLarge = (M1,sizeFactor XLarge)
instanceClass M2_2XLarge = (M2,sizeFactor XLarge2X)
instanceClass M2_4XLarge = (M2,sizeFactor XLarge4X)
instanceClass M2_XLarge = (M2,sizeFactor XLarge)
instanceClass M3_2XLarge = (M3,sizeFactor XLarge2X)
instanceClass M3_Large = (M3,sizeFactor Large)
instanceClass M3_Medium = (M3,sizeFactor Medium)
instanceClass M3_XLarge = (M3,sizeFactor XLarge)
instanceClass R3_2XLarge = (R3,sizeFactor XLarge2X)
instanceClass R3_4XLarge = (R3,sizeFactor XLarge4X)
instanceClass R3_8XLarge = (R3,sizeFactor XLarge8X)
instanceClass R3_Large = (R3,sizeFactor Large)
instanceClass R3_XLarge = (R3,sizeFactor XLarge)
instanceClass T1_Micro = (T1,sizeFactor Micro)
instanceClass T2_Medium = (T2,sizeFactor Medium)
instanceClass T2_Micro = (T2,sizeFactor Micro)
instanceClass T2_Small = (T2,sizeFactor Small)

sizeFactor :: forall a.
              Fractional a
           => InstanceSize -> a
sizeFactor Micro    = 0.5
sizeFactor Small    = 1
sizeFactor Medium   = 2
sizeFactor Large    = 4
sizeFactor XLarge   = 8
sizeFactor XLarge2X = 16
sizeFactor XLarge4X = 32
sizeFactor XLarge8X = 64
