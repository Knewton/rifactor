{-# LANGUAGE NoImplicitPrelude #-}

-- Module      : Rifactor.Types.AWS2
-- Copyright   : (c) 2015 Knewton, Inc <se@knewton.com>
--               (c) 2015 Tim Dysinger <tim@dysinger.net> (contributor)
-- License     : Apache 2.0 http://opensource.org/licenses/Apache-2.0
-- Maintainer  : Tim Dysinger <tim@dysinger.net>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Rifactor.Types.AWS2 where

import           BasePrelude ()
import qualified Network.AWS as AWS
import qualified Network.AWS.EC2 as EC2
import           Rifactor.Types.Model2

type AwsModel = Manifest (Resource AWS.Env EC2.ReservedInstances EC2.Instance)
