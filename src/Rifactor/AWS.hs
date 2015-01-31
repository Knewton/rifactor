{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

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
initEnvs cfg lgr =
  for [(a,r) | r <- (cfg ^. regions)
             , a <- (cfg ^. accounts)]
      (\(a,r) ->
         (getEnv r
                 (FromKeys (AccessKey (B.pack (a ^. accessKey)))
                           (SecretKey (B.pack (a ^. secretKey)))) <&>
          (envLogger .~ lgr)))

-- | Query for pending ReservedInstancesModifications.  We care about
-- this because we don't want to do anything else until these are no
-- longer pending.  If we do have a pending, just blow out with an ಠ_ಠ
-- (error & stops the AWS monad from doing anything further).
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
                           if (not . null) rims
                              then pure ()
                              else ಠ_ಠ "There are pending RI modifications."))

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
                 pure (map (Reserved e) xs))

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
