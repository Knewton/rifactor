{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.ModifyReservedInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Modifies the Availability Zone, instance count, instance type, or network
-- platform (EC2-Classic or EC2-VPC) of your Reserved Instances. The Reserved
-- Instances to be modified must be identical, except for Availability Zone,
-- network platform, and instance type.
--
-- For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-modifying.html Modifying Reserved Instances> in the Amazon Elastic
-- Compute Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifyReservedInstances.html>
module Network.AWS.EC2.ModifyReservedInstances
    (
    -- * Request
      ModifyReservedInstances
    -- ** Request constructor
    , modifyReservedInstances
    -- ** Request lenses
    , mriClientToken
    , mriReservedInstancesIds
    , mriTargetConfigurations

    -- * Response
    , ModifyReservedInstancesResponse
    -- ** Response constructor
    , modifyReservedInstancesResponse
    -- ** Response lenses
    , mrirReservedInstancesModificationId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data ModifyReservedInstances = ModifyReservedInstances
    { _mriClientToken          :: Maybe Text
    , _mriReservedInstancesIds :: List "ReservedInstancesId" Text
    , _mriTargetConfigurations :: List "item" ReservedInstancesConfiguration
    } deriving (Eq, Read, Show)

-- | 'ModifyReservedInstances' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mriClientToken' @::@ 'Maybe' 'Text'
--
-- * 'mriReservedInstancesIds' @::@ ['Text']
--
-- * 'mriTargetConfigurations' @::@ ['ReservedInstancesConfiguration']
--
modifyReservedInstances :: ModifyReservedInstances
modifyReservedInstances = ModifyReservedInstances
    { _mriClientToken          = Nothing
    , _mriReservedInstancesIds = mempty
    , _mriTargetConfigurations = mempty
    }

-- | A unique, case-sensitive token you provide to ensure idempotency of your
-- modification request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
mriClientToken :: Lens' ModifyReservedInstances (Maybe Text)
mriClientToken = lens _mriClientToken (\s a -> s { _mriClientToken = a })

-- | The IDs of the Reserved Instances to modify.
mriReservedInstancesIds :: Lens' ModifyReservedInstances [Text]
mriReservedInstancesIds =
    lens _mriReservedInstancesIds (\s a -> s { _mriReservedInstancesIds = a })
        . _List

-- | The configuration settings for the Reserved Instances to modify.
mriTargetConfigurations :: Lens' ModifyReservedInstances [ReservedInstancesConfiguration]
mriTargetConfigurations =
    lens _mriTargetConfigurations (\s a -> s { _mriTargetConfigurations = a })
        . _List

newtype ModifyReservedInstancesResponse = ModifyReservedInstancesResponse
    { _mrirReservedInstancesModificationId :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'ModifyReservedInstancesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mrirReservedInstancesModificationId' @::@ 'Maybe' 'Text'
--
modifyReservedInstancesResponse :: ModifyReservedInstancesResponse
modifyReservedInstancesResponse = ModifyReservedInstancesResponse
    { _mrirReservedInstancesModificationId = Nothing
    }

-- | The ID for the modification.
mrirReservedInstancesModificationId :: Lens' ModifyReservedInstancesResponse (Maybe Text)
mrirReservedInstancesModificationId =
    lens _mrirReservedInstancesModificationId
        (\s a -> s { _mrirReservedInstancesModificationId = a })

instance ToPath ModifyReservedInstances where
    toPath = const "/"

instance ToQuery ModifyReservedInstances where
    toQuery ModifyReservedInstances{..} = mconcat
        [ "ClientToken"                               =? _mriClientToken
        , "ReservedInstancesId"                       `toQueryList` _mriReservedInstancesIds
        , "ReservedInstancesConfigurationSetItemType" `toQueryList` _mriTargetConfigurations
        ]

instance ToHeaders ModifyReservedInstances

instance AWSRequest ModifyReservedInstances where
    type Sv ModifyReservedInstances = EC2
    type Rs ModifyReservedInstances = ModifyReservedInstancesResponse

    request  = post "ModifyReservedInstances"
    response = xmlResponse

instance FromXML ModifyReservedInstancesResponse where
    parseXML x = ModifyReservedInstancesResponse
        <$> x .@? "reservedInstancesModificationId"
