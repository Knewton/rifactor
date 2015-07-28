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

-- Module      : Network.AWS.EC2.DescribeRegions
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

-- | Describes one or more regions that are currently available to you.
--
-- For a list of the regions supported by Amazon EC2, see <http://docs.aws.amazon.com/general/latest/gr/rande.html#ec2_region Regions and Endpoints>.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeRegions.html>
module Network.AWS.EC2.DescribeRegions
    (
    -- * Request
      DescribeRegions
    -- ** Request constructor
    , describeRegions
    -- ** Request lenses
    , dr1DryRun
    , dr1Filters
    , dr1RegionNames

    -- * Response
    , DescribeRegionsResponse
    -- ** Response constructor
    , describeRegionsResponse
    -- ** Response lenses
    , drrRegions
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeRegions = DescribeRegions
    { _dr1DryRun      :: Maybe Bool
    , _dr1Filters     :: List "Filter" Filter
    , _dr1RegionNames :: List "RegionName" Text
    } deriving (Eq, Read, Show)

-- | 'DescribeRegions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dr1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dr1Filters' @::@ ['Filter']
--
-- * 'dr1RegionNames' @::@ ['Text']
--
describeRegions :: DescribeRegions
describeRegions = DescribeRegions
    { _dr1DryRun      = Nothing
    , _dr1RegionNames = mempty
    , _dr1Filters     = mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
dr1DryRun :: Lens' DescribeRegions (Maybe Bool)
dr1DryRun = lens _dr1DryRun (\s a -> s { _dr1DryRun = a })

-- | One or more filters.
--
-- 'endpoint' - The endpoint of the region (for example, 'ec2.us-east-1.amazonaws.com').
--
-- 'region-name' - The name of the region (for example, 'us-east-1').
--
--
dr1Filters :: Lens' DescribeRegions [Filter]
dr1Filters = lens _dr1Filters (\s a -> s { _dr1Filters = a }) . _List

-- | The names of one or more regions.
dr1RegionNames :: Lens' DescribeRegions [Text]
dr1RegionNames = lens _dr1RegionNames (\s a -> s { _dr1RegionNames = a }) . _List

newtype DescribeRegionsResponse = DescribeRegionsResponse
    { _drrRegions :: List "item" Region
    } deriving (Eq, Read, Show, Monoid, Semigroup)

-- | 'DescribeRegionsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drrRegions' @::@ ['Region']
--
describeRegionsResponse :: DescribeRegionsResponse
describeRegionsResponse = DescribeRegionsResponse
    { _drrRegions = mempty
    }

-- | Information about one or more regions.
drrRegions :: Lens' DescribeRegionsResponse [Region]
drrRegions = lens _drrRegions (\s a -> s { _drrRegions = a }) . _List

instance ToPath DescribeRegions where
    toPath = const "/"

instance ToQuery DescribeRegions where
    toQuery DescribeRegions{..} = mconcat
        [ "DryRun"     =? _dr1DryRun
        , "Filter"     `toQueryList` _dr1Filters
        , "RegionName" `toQueryList` _dr1RegionNames
        ]

instance ToHeaders DescribeRegions

instance AWSRequest DescribeRegions where
    type Sv DescribeRegions = EC2
    type Rs DescribeRegions = DescribeRegionsResponse

    request  = post "DescribeRegions"
    response = xmlResponse

instance FromXML DescribeRegionsResponse where
    parseXML x = DescribeRegionsResponse
        <$> x .@? "regionInfo" .!@ mempty
