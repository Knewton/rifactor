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

-- Module      : Network.AWS.EC2.DescribeReservedInstances
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

-- | Describes one or more of the Reserved Instances that you purchased.
--
-- For more information about Reserved Instances, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/concepts-on-demand-reserved-instances.html Reserved Instances> in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeReservedInstances.html>
module Network.AWS.EC2.DescribeReservedInstances
    (
    -- * Request
      DescribeReservedInstances
    -- ** Request constructor
    , describeReservedInstances
    -- ** Request lenses
    , driDryRun
    , driFilters
    , driOfferingType
    , driReservedInstancesIds

    -- * Response
    , DescribeReservedInstancesResponse
    -- ** Response constructor
    , describeReservedInstancesResponse
    -- ** Response lenses
    , drirReservedInstances
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeReservedInstances = DescribeReservedInstances
    { _driDryRun               :: Maybe Bool
    , _driFilters              :: List "Filter" Filter
    , _driOfferingType         :: Maybe OfferingTypeValues
    , _driReservedInstancesIds :: List "ReservedInstancesId" Text
    } deriving (Eq, Read, Show)

-- | 'DescribeReservedInstances' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'driDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'driFilters' @::@ ['Filter']
--
-- * 'driOfferingType' @::@ 'Maybe' 'OfferingTypeValues'
--
-- * 'driReservedInstancesIds' @::@ ['Text']
--
describeReservedInstances :: DescribeReservedInstances
describeReservedInstances = DescribeReservedInstances
    { _driDryRun               = Nothing
    , _driReservedInstancesIds = mempty
    , _driFilters              = mempty
    , _driOfferingType         = Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
driDryRun :: Lens' DescribeReservedInstances (Maybe Bool)
driDryRun = lens _driDryRun (\s a -> s { _driDryRun = a })

-- | One or more filters.
--
-- 'availability-zone' - The Availability Zone where the Reserved Instance can
-- be used.
--
-- 'duration' - The duration of the Reserved Instance (one year or three
-- years), in seconds ('31536000' | '94608000').
--
-- 'end' - The time when the Reserved Instance expires (for example,
-- 2014-08-07T11:54:42.000Z).
--
-- 'fixed-price' - The purchase price of the Reserved Instance (for example,
-- 9800.0).
--
-- 'instance-type' - The instance type on which the Reserved Instance can be
-- used.
--
-- 'product-description' - The product description of the Reserved Instance ('Linux/UNIX' | 'Linux/UNIX (Amazon VPC)' | 'Windows' | 'Windows (Amazon VPC)').
--
-- 'reserved-instances-id' - The ID of the Reserved Instance.
--
-- 'start' - The time at which the Reserved Instance purchase request was
-- placed (for example, 2014-08-07T11:54:42.000Z).
--
-- 'state' - The state of the Reserved Instance ('payment-pending' | 'active' | 'payment-failed' | 'retired').
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
-- 'usage-price' - The usage price of the Reserved Instance, per hour (for
-- example, 0.84).
--
--
driFilters :: Lens' DescribeReservedInstances [Filter]
driFilters = lens _driFilters (\s a -> s { _driFilters = a }) . _List

-- | The Reserved Instance offering type. If you are using tools that predate the
-- 2011-11-01 API version, you only have access to the 'Medium Utilization'
-- Reserved Instance offering type.
driOfferingType :: Lens' DescribeReservedInstances (Maybe OfferingTypeValues)
driOfferingType = lens _driOfferingType (\s a -> s { _driOfferingType = a })

-- | One or more Reserved Instance IDs.
--
-- Default: Describes all your Reserved Instances, or only those otherwise
-- specified.
driReservedInstancesIds :: Lens' DescribeReservedInstances [Text]
driReservedInstancesIds =
    lens _driReservedInstancesIds (\s a -> s { _driReservedInstancesIds = a })
        . _List

newtype DescribeReservedInstancesResponse = DescribeReservedInstancesResponse
    { _drirReservedInstances :: List "item" ReservedInstances
    } deriving (Eq, Read, Show, Monoid, Semigroup)

-- | 'DescribeReservedInstancesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drirReservedInstances' @::@ ['ReservedInstances']
--
describeReservedInstancesResponse :: DescribeReservedInstancesResponse
describeReservedInstancesResponse = DescribeReservedInstancesResponse
    { _drirReservedInstances = mempty
    }

-- | A list of Reserved Instances.
drirReservedInstances :: Lens' DescribeReservedInstancesResponse [ReservedInstances]
drirReservedInstances =
    lens _drirReservedInstances (\s a -> s { _drirReservedInstances = a })
        . _List

instance ToPath DescribeReservedInstances where
    toPath = const "/"

instance ToQuery DescribeReservedInstances where
    toQuery DescribeReservedInstances{..} = mconcat
        [ "DryRun"              =? _driDryRun
        , "Filter"              `toQueryList` _driFilters
        , "OfferingType"        =? _driOfferingType
        , "ReservedInstancesId" `toQueryList` _driReservedInstancesIds
        ]

instance ToHeaders DescribeReservedInstances

instance AWSRequest DescribeReservedInstances where
    type Sv DescribeReservedInstances = EC2
    type Rs DescribeReservedInstances = DescribeReservedInstancesResponse

    request  = post "DescribeReservedInstances"
    response = xmlResponse

instance FromXML DescribeReservedInstancesResponse where
    parseXML x = DescribeReservedInstancesResponse
        <$> x .@? "reservedInstancesSet" .!@ mempty
