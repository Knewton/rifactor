{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import           Data.Aeson ((.=), ToJSON, object, toJSON)
import           Data.Aeson.TH (deriveToJSON)
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.AWS hiding (Env)
import qualified Network.AWS as AWS (Env)
import qualified Network.AWS.Data as AWS (toText)
import           Network.AWS.EC2 hiding (Region,Instance)
import qualified Network.AWS.EC2 as EC2 (Instance)
import           Rifactor.Types.Internal
import           Rifactor.Types.Model

default (Text)

type AwsModel = Model AWS.Env ReservedInstances EC2.Instance
type AwsModelTransition = Transition AwsModel

type AwsEnv = Env AWS.Env
type AwsInstance = Instance AWS.Env EC2.Instance
type AwsReserved = Reserved AWS.Env ReservedInstances

type AwsUsedReserved = Used AwsReserved AwsInstance

type AwsSplitReserved = Split AwsReserved AwsInstance
type AwsSplitUsedReserved = Split AwsUsedReserved AwsInstance

type AwsCombineReserved = Combine AwsReserved

mkAwsModel :: [AwsInstance] -> [AwsReserved] -> AwsModel
mkAwsModel is rs = Model is rs [] [] [] []

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
  | R3
  | T1
  | T2
  deriving (Show,Eq,Enum)

data ISize
  = Micro
  | Small
  | Medium
  | Large
  | XLarge
  | XLarge2X
  | XLarge4X
  | XLarge8X
  deriving (Show,Eq,Enum)

data IType =
  IType {_insGroup :: IGroup
        ,_insType :: InstanceType
        ,_insFactor :: Float}
  deriving (Show,Eq)

{- Eq -}

instance Eq AWS.Env where
  (==) e0 e1 = (e0 ^. envRegion == e1 ^. envRegion)

instance Show AWS.Env where
  show e = T.unpack (AWS.toText (e ^. envRegion))

{- LENS -}

$(makeLenses ''IType)

{- JSON -}

instance ToJSON AWS.Env where
  toJSON e = object ["region" .= (e ^. envRegion)]

$(deriveToJSON deriveOptions ''Region)
$(deriveToJSON deriveOptions ''ArchitectureValues)
$(deriveToJSON deriveOptions ''AttachmentStatus)
$(deriveToJSON deriveOptions ''BlockDeviceMapping)
$(deriveToJSON deriveOptions ''CurrencyCodeValues)
$(deriveToJSON deriveOptions ''DeviceType)
$(deriveToJSON deriveOptions ''EbsBlockDevice)
$(deriveToJSON deriveOptions ''EbsInstanceBlockDevice)
$(deriveToJSON deriveOptions ''GroupIdentifier)
$(deriveToJSON deriveOptions ''HypervisorType)
$(deriveToJSON deriveOptions ''IamInstanceProfile)
$(deriveToJSON deriveOptions ''EC2.Instance)
$(deriveToJSON deriveOptions ''InstanceBlockDeviceMapping)
$(deriveToJSON deriveOptions ''InstanceLifecycleType)
$(deriveToJSON deriveOptions ''InstanceNetworkInterface)
$(deriveToJSON deriveOptions ''InstanceNetworkInterfaceAssociation)
$(deriveToJSON deriveOptions ''InstanceNetworkInterfaceAttachment)
$(deriveToJSON deriveOptions ''InstancePrivateIpAddress)
$(deriveToJSON deriveOptions ''InstanceState)
$(deriveToJSON deriveOptions ''InstanceStateName)
$(deriveToJSON deriveOptions ''InstanceType)
$(deriveToJSON deriveOptions ''Monitoring)
$(deriveToJSON deriveOptions ''MonitoringState)
$(deriveToJSON deriveOptions ''NetworkInterfaceStatus)
$(deriveToJSON deriveOptions ''OfferingTypeValues)
$(deriveToJSON deriveOptions ''Placement)
$(deriveToJSON deriveOptions ''PlatformValues)
$(deriveToJSON deriveOptions ''ProductCode)
$(deriveToJSON deriveOptions ''ProductCodeValues)
$(deriveToJSON deriveOptions ''RIProductDescription)
$(deriveToJSON deriveOptions ''RecurringCharge)
$(deriveToJSON deriveOptions ''RecurringChargeFrequency)
$(deriveToJSON deriveOptions ''ReservedInstanceState)
$(deriveToJSON deriveOptions ''ReservedInstances)
$(deriveToJSON deriveOptions ''ReservedInstancesConfiguration)
$(deriveToJSON deriveOptions ''ReservedInstancesId)
$(deriveToJSON deriveOptions ''ReservedInstancesModification)
$(deriveToJSON deriveOptions ''ReservedInstancesModificationResult)
$(deriveToJSON deriveOptions ''StateReason)
$(deriveToJSON deriveOptions ''Tag)
$(deriveToJSON deriveOptions ''Tenancy)
$(deriveToJSON deriveOptions ''VirtualizationType)
$(deriveToJSON deriveOptions ''VolumeType)
