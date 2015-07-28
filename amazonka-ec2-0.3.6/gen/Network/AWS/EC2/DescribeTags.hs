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

-- Module      : Network.AWS.EC2.DescribeTags
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

-- | Describes one or more of the tags for your EC2 resources.
--
-- For more information about tags, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging Your Resources> in the /AmazonElastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeTags.html>
module Network.AWS.EC2.DescribeTags
    (
    -- * Request
      DescribeTags
    -- ** Request constructor
    , describeTags
    -- ** Request lenses
    , dtDryRun
    , dtFilters
    , dtMaxResults
    , dtNextToken

    -- * Response
    , DescribeTagsResponse
    -- ** Response constructor
    , describeTagsResponse
    -- ** Response lenses
    , dtrNextToken
    , dtrTags
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeTags = DescribeTags
    { _dtDryRun     :: Maybe Bool
    , _dtFilters    :: List "Filter" Filter
    , _dtMaxResults :: Maybe Int
    , _dtNextToken  :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'DescribeTags' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dtFilters' @::@ ['Filter']
--
-- * 'dtMaxResults' @::@ 'Maybe' 'Int'
--
-- * 'dtNextToken' @::@ 'Maybe' 'Text'
--
describeTags :: DescribeTags
describeTags = DescribeTags
    { _dtDryRun     = Nothing
    , _dtFilters    = mempty
    , _dtMaxResults = Nothing
    , _dtNextToken  = Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
dtDryRun :: Lens' DescribeTags (Maybe Bool)
dtDryRun = lens _dtDryRun (\s a -> s { _dtDryRun = a })

-- | One or more filters.
--
-- 'key' - The tag key.
--
-- 'resource-id' - The resource ID.
--
-- 'resource-type' - The resource type ('customer-gateway' | 'dhcp-options' | 'image'
-- | 'instance' | 'internet-gateway' | 'network-acl' | 'network-interface' | 'reserved-instances' | 'route-table' | 'security-group' | 'snapshot' | 'spot-instances-request' | 'subnet'
-- | 'volume' | 'vpc' | 'vpn-connection' | 'vpn-gateway').
--
-- 'value' - The tag value.
--
--
dtFilters :: Lens' DescribeTags [Filter]
dtFilters = lens _dtFilters (\s a -> s { _dtFilters = a }) . _List

-- | The maximum number of results to return for the request in a single page. The
-- remaining results of the initial request can be seen by sending another
-- request with the returned 'NextToken' value. This value can be between 5 and
-- 1000; if 'MaxResults' is given a value larger than 1000, only 1000 results are
-- returned.
dtMaxResults :: Lens' DescribeTags (Maybe Int)
dtMaxResults = lens _dtMaxResults (\s a -> s { _dtMaxResults = a })

-- | The token to retrieve the next page of results.
dtNextToken :: Lens' DescribeTags (Maybe Text)
dtNextToken = lens _dtNextToken (\s a -> s { _dtNextToken = a })

data DescribeTagsResponse = DescribeTagsResponse
    { _dtrNextToken :: Maybe Text
    , _dtrTags      :: List "item" TagDescription
    } deriving (Eq, Read, Show)

-- | 'DescribeTagsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtrNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dtrTags' @::@ ['TagDescription']
--
describeTagsResponse :: DescribeTagsResponse
describeTagsResponse = DescribeTagsResponse
    { _dtrTags      = mempty
    , _dtrNextToken = Nothing
    }

-- | The token to use to retrieve the next page of results. This value is 'null'
-- when there are no more results to return..
dtrNextToken :: Lens' DescribeTagsResponse (Maybe Text)
dtrNextToken = lens _dtrNextToken (\s a -> s { _dtrNextToken = a })

-- | A list of tags.
dtrTags :: Lens' DescribeTagsResponse [TagDescription]
dtrTags = lens _dtrTags (\s a -> s { _dtrTags = a }) . _List

instance ToPath DescribeTags where
    toPath = const "/"

instance ToQuery DescribeTags where
    toQuery DescribeTags{..} = mconcat
        [ "DryRun"     =? _dtDryRun
        , "Filter"     `toQueryList` _dtFilters
        , "MaxResults" =? _dtMaxResults
        , "NextToken"  =? _dtNextToken
        ]

instance ToHeaders DescribeTags

instance AWSRequest DescribeTags where
    type Sv DescribeTags = EC2
    type Rs DescribeTags = DescribeTagsResponse

    request  = post "DescribeTags"
    response = xmlResponse

instance FromXML DescribeTagsResponse where
    parseXML x = DescribeTagsResponse
        <$> x .@? "nextToken"
        <*> x .@? "tagSet" .!@ mempty

instance AWSPager DescribeTags where
    page rq rs
        | stop (rs ^. dtrNextToken) = Nothing
        | otherwise = (\x -> rq & dtNextToken ?~ x)
            <$> (rs ^. dtrNextToken)
