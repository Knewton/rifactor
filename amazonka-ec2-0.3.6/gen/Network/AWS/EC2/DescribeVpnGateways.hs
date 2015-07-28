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

-- Module      : Network.AWS.EC2.DescribeVpnGateways
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

-- | Describes one or more of your virtual private gateways.
--
-- For more information about virtual private gateways, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_VPN.html Adding an IPsecHardware VPN to Your VPC> in the /Amazon Virtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeVpnGateways.html>
module Network.AWS.EC2.DescribeVpnGateways
    (
    -- * Request
      DescribeVpnGateways
    -- ** Request constructor
    , describeVpnGateways
    -- ** Request lenses
    , dvg2DryRun
    , dvg2Filters
    , dvg2VpnGatewayIds

    -- * Response
    , DescribeVpnGatewaysResponse
    -- ** Response constructor
    , describeVpnGatewaysResponse
    -- ** Response lenses
    , dvgrVpnGateways
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeVpnGateways = DescribeVpnGateways
    { _dvg2DryRun        :: Maybe Bool
    , _dvg2Filters       :: List "Filter" Filter
    , _dvg2VpnGatewayIds :: List "VpnGatewayId" Text
    } deriving (Eq, Read, Show)

-- | 'DescribeVpnGateways' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvg2DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dvg2Filters' @::@ ['Filter']
--
-- * 'dvg2VpnGatewayIds' @::@ ['Text']
--
describeVpnGateways :: DescribeVpnGateways
describeVpnGateways = DescribeVpnGateways
    { _dvg2DryRun        = Nothing
    , _dvg2VpnGatewayIds = mempty
    , _dvg2Filters       = mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
dvg2DryRun :: Lens' DescribeVpnGateways (Maybe Bool)
dvg2DryRun = lens _dvg2DryRun (\s a -> s { _dvg2DryRun = a })

-- | One or more filters.
--
-- 'attachment.state' - The current state of the attachment between the gateway
-- and the VPC ('attaching' | 'attached' | 'detaching' | 'detached').
--
-- 'attachment.vpc-id' - The ID of an attached VPC.
--
-- 'availability-zone' - The Availability Zone for the virtual private gateway.
--
-- 'state' - The state of the virtual private gateway ('pending' | 'available' | 'deleting' | 'deleted').
--
-- 'tag':/key/=/value/ - The key/value combination of a tag assigned to the
-- resource.
--
-- 'tag-key' - The key of a tag assigned to the resource. This filter is
-- independent of the 'tag-value' filter. For example, if you use both the filter
-- "tag-key=Purpose" and the filter "tag-value=X", you get any resources
-- assigned both the tag key Purpose (regardless of what the tag's value is),
-- and the tag value X (regardless of what the tag's key is). If you want to
-- list only resources where Purpose is X, see the 'tag':/key/=/value/ filter.
--
-- 'tag-value' - The value of a tag assigned to the resource. This filter is
-- independent of the 'tag-key' filter.
--
-- 'type' - The type of virtual private gateway. Currently the only supported
-- type is 'ipsec.1'.
--
-- 'vpn-gateway-id' - The ID of the virtual private gateway.
--
--
dvg2Filters :: Lens' DescribeVpnGateways [Filter]
dvg2Filters = lens _dvg2Filters (\s a -> s { _dvg2Filters = a }) . _List

-- | One or more virtual private gateway IDs.
--
-- Default: Describes all your virtual private gateways.
dvg2VpnGatewayIds :: Lens' DescribeVpnGateways [Text]
dvg2VpnGatewayIds =
    lens _dvg2VpnGatewayIds (\s a -> s { _dvg2VpnGatewayIds = a })
        . _List

newtype DescribeVpnGatewaysResponse = DescribeVpnGatewaysResponse
    { _dvgrVpnGateways :: List "item" VpnGateway
    } deriving (Eq, Read, Show, Monoid, Semigroup)

-- | 'DescribeVpnGatewaysResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvgrVpnGateways' @::@ ['VpnGateway']
--
describeVpnGatewaysResponse :: DescribeVpnGatewaysResponse
describeVpnGatewaysResponse = DescribeVpnGatewaysResponse
    { _dvgrVpnGateways = mempty
    }

-- | Information about one or more virtual private gateways.
dvgrVpnGateways :: Lens' DescribeVpnGatewaysResponse [VpnGateway]
dvgrVpnGateways = lens _dvgrVpnGateways (\s a -> s { _dvgrVpnGateways = a }) . _List

instance ToPath DescribeVpnGateways where
    toPath = const "/"

instance ToQuery DescribeVpnGateways where
    toQuery DescribeVpnGateways{..} = mconcat
        [ "DryRun"       =? _dvg2DryRun
        , "Filter"       `toQueryList` _dvg2Filters
        , "VpnGatewayId" `toQueryList` _dvg2VpnGatewayIds
        ]

instance ToHeaders DescribeVpnGateways

instance AWSRequest DescribeVpnGateways where
    type Sv DescribeVpnGateways = EC2
    type Rs DescribeVpnGateways = DescribeVpnGatewaysResponse

    request  = post "DescribeVpnGateways"
    response = xmlResponse

instance FromXML DescribeVpnGatewaysResponse where
    parseXML x = DescribeVpnGatewaysResponse
        <$> x .@? "vpnGatewaySet" .!@ mempty
