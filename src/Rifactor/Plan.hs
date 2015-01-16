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

say :: Show a => Build.Builder -> a -> AWST IO ()
say msg = info . mappend msg . Build.stringUtf8 . show

getTimestamp :: IO Integer
getTimestamp = truncate <$> getPOSIXTime
