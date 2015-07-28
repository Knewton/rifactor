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

-- Module      : Network.AWS.EC2.DescribeRouteTables
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

-- | Describes one or more of your route tables.
--
-- For more information about route tables, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Route_Tables.html Route Tables> in the /AmazonVirtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeRouteTables.html>
module Network.AWS.EC2.DescribeRouteTables
    (
    -- * Request
      DescribeRouteTables
    -- ** Request constructor
    , describeRouteTables
    -- ** Request lenses
    , drt2DryRun
    , drt2Filters
    , drt2RouteTableIds

    -- * Response
    , DescribeRouteTablesResponse
    -- ** Response constructor
    , describeRouteTablesResponse
    -- ** Response lenses
    , drtrRouteTables
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeRouteTables = DescribeRouteTables
    { _drt2DryRun        :: Maybe Bool
    , _drt2Filters       :: List "Filter" Filter
    , _drt2RouteTableIds :: List "item" Text
    } deriving (Eq, Read, Show)

-- | 'DescribeRouteTables' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drt2DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'drt2Filters' @::@ ['Filter']
--
-- * 'drt2RouteTableIds' @::@ ['Text']
--
describeRouteTables :: DescribeRouteTables
describeRouteTables = DescribeRouteTables
    { _drt2DryRun        = Nothing
    , _drt2RouteTableIds = mempty
    , _drt2Filters       = mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
drt2DryRun :: Lens' DescribeRouteTables (Maybe Bool)
drt2DryRun = lens _drt2DryRun (\s a -> s { _drt2DryRun = a })

-- | One or more filters.
--
-- 'association.route-table-association-id' - The ID of an association ID for
-- the route table.
--
-- 'association.route-table-id' - The ID of the route table involved in the
-- association.
--
-- 'association.subnet-id' - The ID of the subnet involved in the association.
--
-- 'association.main' - Indicates whether the route table is the main route
-- table for the VPC.
--
-- 'route-table-id' - The ID of the route table.
--
-- 'route.destination-cidr-block' - The CIDR range specified in a route in the
-- table.
--
-- 'route.destination-prefix-list-id' - The ID (prefix) of the AWS service
-- specified in a route in the table.
--
-- 'route.gateway-id' - The ID of a gateway specified in a route in the table.
--
-- 'route.instance-id' - The ID of an instance specified in a route in the
-- table.
--
-- 'route.origin' - Describes how the route was created. 'CreateRouteTable'
-- indicates that the route was automatically created when the route table was
-- created; 'CreateRoute' indicates that the route was manually added to the route
-- table; 'EnableVgwRoutePropagation' indicates that the route was propagated by
-- route propagation.
--
-- 'route.state' - The state of a route in the route table ('active' | 'blackhole'). The blackhole state indicates that the route's target isn't available (for example, the specified gateway isn't attached to the VPC, the specified NAT instance has been terminated, and so on).
--
--
-- 'route.vpc-peering-connection-id' - The ID of a VPC peering connection
-- specified in a route in the table.
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
-- 'vpc-id' - The ID of the VPC for the route table.
--
--
drt2Filters :: Lens' DescribeRouteTables [Filter]
drt2Filters = lens _drt2Filters (\s a -> s { _drt2Filters = a }) . _List

-- | One or more route table IDs.
--
-- Default: Describes all your route tables.
drt2RouteTableIds :: Lens' DescribeRouteTables [Text]
drt2RouteTableIds =
    lens _drt2RouteTableIds (\s a -> s { _drt2RouteTableIds = a })
        . _List

newtype DescribeRouteTablesResponse = DescribeRouteTablesResponse
    { _drtrRouteTables :: List "item" RouteTable
    } deriving (Eq, Read, Show, Monoid, Semigroup)

-- | 'DescribeRouteTablesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drtrRouteTables' @::@ ['RouteTable']
--
describeRouteTablesResponse :: DescribeRouteTablesResponse
describeRouteTablesResponse = DescribeRouteTablesResponse
    { _drtrRouteTables = mempty
    }

-- | Information about one or more route tables.
drtrRouteTables :: Lens' DescribeRouteTablesResponse [RouteTable]
drtrRouteTables = lens _drtrRouteTables (\s a -> s { _drtrRouteTables = a }) . _List

instance ToPath DescribeRouteTables where
    toPath = const "/"

instance ToQuery DescribeRouteTables where
    toQuery DescribeRouteTables{..} = mconcat
        [ "DryRun"       =? _drt2DryRun
        , "Filter"       `toQueryList` _drt2Filters
        , "RouteTableId" `toQueryList` _drt2RouteTableIds
        ]

instance ToHeaders DescribeRouteTables

instance AWSRequest DescribeRouteTables where
    type Sv DescribeRouteTables = EC2
    type Rs DescribeRouteTables = DescribeRouteTablesResponse

    request  = post "DescribeRouteTables"
    response = xmlResponse

instance FromXML DescribeRouteTablesResponse where
    parseXML x = DescribeRouteTablesResponse
        <$> x .@? "routeTableSet" .!@ mempty
