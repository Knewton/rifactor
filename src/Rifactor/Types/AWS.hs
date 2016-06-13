{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Module      : Rifactor.Types.AWS
-- Copyright   : (c) 2015 Knewton, Inc <se@knewton.com>
--               (c) 2015 Tim Dysinger <tim@dysinger.net> (contributor)
-- License     : Apache 2.0 http://opensource.org/licenses/Apache-2.0
-- Maintainer  : Tim Dysinger <tim@dysinger.net>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Rifactor.Types.AWS where

import           BasePrelude
import           Control.Lens hiding ((.=))
import           Data.Text (Text)
import qualified Network.AWS as AWS
import qualified Network.AWS.EC2 as EC2
import           Rifactor.Types.Model

default (Text)

type AwsEnv = Env AWS.Env
type AwsResource = Resource AWS.Env EC2.ReservedInstances EC2.Instance
type AwsPlan = Plan AwsResource
type AwsPlanTransition = Transition AwsPlan

data IGroup
  = C1
  | C2
  | C3
  | C4
  | CC1
  | CC2
  | CG1
  | CR1
  | G2
  | HI1
  | HS1
  | HS2
  | I2
  | M1
  | M2
  | M3
  | M4
  | R3
  | T1
  | T2
  deriving (Enum,Eq,Ord,Show)

data ISize
  = Micro
  | Small
  | Medium
  | Large
  | XLarge
  | XLarge2X
  | XLarge4X
  | XLarge8X
  | XLarge10X
  deriving (Enum,Eq,Ord,Show)

data IType =
  IType {_insGroup :: IGroup
        ,_insType :: EC2.InstanceType
        ,_insFactor :: Double}
  deriving (Eq,Ord,Show)

$(makeLenses ''IType)
