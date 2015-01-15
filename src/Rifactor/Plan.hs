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
-- import           Control.Monad
-- import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS
import qualified Data.ByteString.Builder as Build
import           Data.Monoid
import qualified Data.Text as Text
-- import qualified Data.Text.IO as Text
import           Data.Time.Clock.POSIX
-- import           Network.AWS.EC2
import           Rifactor.Types
import           System.IO

plan :: Options -> IO (Either Error ())
plan _opts =
  do lgr <- newLogger Debug stdout
     -- TODO 16 accounts w/ IAM creds for each
     env <- getEnv NorthVirginia Discover <&>
            envLogger .~
            lgr
     ts <- Text.pack . show <$> getTimestamp
     runAWST env $
       do say "Listing Instances #" ts
          -- TODO list instances
          -- TODO list reservations
          -- TODO plan builder
          -- TODO plan printer
          say "All Done #" ts

say :: Show a => Build.Builder -> a -> AWST IO ()
say msg = info . mappend msg . Build.stringUtf8 . show

getTimestamp :: IO Integer
getTimestamp = truncate <$> getPOSIXTime
