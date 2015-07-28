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

-- Module      : Network.AWS.EC2.AllocateAddress
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

-- | Acquires an Elastic IP address.
--
-- An Elastic IP address is for use either in the EC2-Classic platform or in a
-- VPC. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP Addresses> in the /Amazon ElasticCompute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AllocateAddress.html>
module Network.AWS.EC2.AllocateAddress
    (
    -- * Request
      AllocateAddress
    -- ** Request constructor
    , allocateAddress
    -- ** Request lenses
    , aaDomain
    , aaDryRun

    -- * Response
    , AllocateAddressResponse
    -- ** Response constructor
    , allocateAddressResponse
    -- ** Response lenses
    , aarAllocationId
    , aarDomain
    , aarPublicIp
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data AllocateAddress = AllocateAddress
    { _aaDomain :: Maybe DomainType
    , _aaDryRun :: Maybe Bool
    } deriving (Eq, Read, Show)

-- | 'AllocateAddress' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aaDomain' @::@ 'Maybe' 'DomainType'
--
-- * 'aaDryRun' @::@ 'Maybe' 'Bool'
--
allocateAddress :: AllocateAddress
allocateAddress = AllocateAddress
    { _aaDryRun = Nothing
    , _aaDomain = Nothing
    }

-- | Set to 'vpc' to allocate the address for use with instances in a VPC.
--
-- Default: The address is for use with instances in EC2-Classic.
aaDomain :: Lens' AllocateAddress (Maybe DomainType)
aaDomain = lens _aaDomain (\s a -> s { _aaDomain = a })

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
aaDryRun :: Lens' AllocateAddress (Maybe Bool)
aaDryRun = lens _aaDryRun (\s a -> s { _aaDryRun = a })

data AllocateAddressResponse = AllocateAddressResponse
    { _aarAllocationId :: Maybe Text
    , _aarDomain       :: Maybe DomainType
    , _aarPublicIp     :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'AllocateAddressResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aarAllocationId' @::@ 'Maybe' 'Text'
--
-- * 'aarDomain' @::@ 'Maybe' 'DomainType'
--
-- * 'aarPublicIp' @::@ 'Maybe' 'Text'
--
allocateAddressResponse :: AllocateAddressResponse
allocateAddressResponse = AllocateAddressResponse
    { _aarPublicIp     = Nothing
    , _aarDomain       = Nothing
    , _aarAllocationId = Nothing
    }

-- | [EC2-VPC] The ID that AWS assigns to represent the allocation of the Elastic
-- IP address for use with instances in a VPC.
aarAllocationId :: Lens' AllocateAddressResponse (Maybe Text)
aarAllocationId = lens _aarAllocationId (\s a -> s { _aarAllocationId = a })

-- | Indicates whether this Elastic IP address is for use with instances in
-- EC2-Classic ('standard') or instances in a VPC ('vpc').
aarDomain :: Lens' AllocateAddressResponse (Maybe DomainType)
aarDomain = lens _aarDomain (\s a -> s { _aarDomain = a })

-- | The Elastic IP address.
aarPublicIp :: Lens' AllocateAddressResponse (Maybe Text)
aarPublicIp = lens _aarPublicIp (\s a -> s { _aarPublicIp = a })

instance ToPath AllocateAddress where
    toPath = const "/"

instance ToQuery AllocateAddress where
    toQuery AllocateAddress{..} = mconcat
        [ "Domain" =? _aaDomain
        , "DryRun" =? _aaDryRun
        ]

instance ToHeaders AllocateAddress

instance AWSRequest AllocateAddress where
    type Sv AllocateAddress = EC2
    type Rs AllocateAddress = AllocateAddressResponse

    request  = post "AllocateAddress"
    response = xmlResponse

instance FromXML AllocateAddressResponse where
    parseXML x = AllocateAddressResponse
        <$> x .@? "allocationId"
        <*> x .@? "domain"
        <*> x .@? "publicIp"
