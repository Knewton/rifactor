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

-- Module      : Network.AWS.EC2.DescribeDhcpOptions
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

-- | Describes one or more of your DHCP options sets.
--
-- For more information about DHCP options sets, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_DHCP_Options.html DHCP Options Sets> in the /Amazon Virtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeDhcpOptions.html>
module Network.AWS.EC2.DescribeDhcpOptions
    (
    -- * Request
      DescribeDhcpOptions
    -- ** Request constructor
    , describeDhcpOptions
    -- ** Request lenses
    , ddoDhcpOptionsIds
    , ddoDryRun
    , ddoFilters

    -- * Response
    , DescribeDhcpOptionsResponse
    -- ** Response constructor
    , describeDhcpOptionsResponse
    -- ** Response lenses
    , ddorDhcpOptions
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeDhcpOptions = DescribeDhcpOptions
    { _ddoDhcpOptionsIds :: List "DhcpOptionsId" Text
    , _ddoDryRun         :: Maybe Bool
    , _ddoFilters        :: List "Filter" Filter
    } deriving (Eq, Read, Show)

-- | 'DescribeDhcpOptions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddoDhcpOptionsIds' @::@ ['Text']
--
-- * 'ddoDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'ddoFilters' @::@ ['Filter']
--
describeDhcpOptions :: DescribeDhcpOptions
describeDhcpOptions = DescribeDhcpOptions
    { _ddoDryRun         = Nothing
    , _ddoDhcpOptionsIds = mempty
    , _ddoFilters        = mempty
    }

-- | The IDs of one or more DHCP options sets.
--
-- Default: Describes all your DHCP options sets.
ddoDhcpOptionsIds :: Lens' DescribeDhcpOptions [Text]
ddoDhcpOptionsIds =
    lens _ddoDhcpOptionsIds (\s a -> s { _ddoDhcpOptionsIds = a })
        . _List

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
ddoDryRun :: Lens' DescribeDhcpOptions (Maybe Bool)
ddoDryRun = lens _ddoDryRun (\s a -> s { _ddoDryRun = a })

-- | One or more filters.
--
-- 'dhcp-options-id' - The ID of a set of DHCP options.
--
-- 'key' - The key for one of the options (for example, 'domain-name').
--
-- 'value' - The value for one of the options.
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
--
ddoFilters :: Lens' DescribeDhcpOptions [Filter]
ddoFilters = lens _ddoFilters (\s a -> s { _ddoFilters = a }) . _List

newtype DescribeDhcpOptionsResponse = DescribeDhcpOptionsResponse
    { _ddorDhcpOptions :: List "item" DhcpOptions
    } deriving (Eq, Read, Show, Monoid, Semigroup)

-- | 'DescribeDhcpOptionsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddorDhcpOptions' @::@ ['DhcpOptions']
--
describeDhcpOptionsResponse :: DescribeDhcpOptionsResponse
describeDhcpOptionsResponse = DescribeDhcpOptionsResponse
    { _ddorDhcpOptions = mempty
    }

-- | Information about one or more DHCP options sets.
ddorDhcpOptions :: Lens' DescribeDhcpOptionsResponse [DhcpOptions]
ddorDhcpOptions = lens _ddorDhcpOptions (\s a -> s { _ddorDhcpOptions = a }) . _List

instance ToPath DescribeDhcpOptions where
    toPath = const "/"

instance ToQuery DescribeDhcpOptions where
    toQuery DescribeDhcpOptions{..} = mconcat
        [ "DhcpOptionsId" `toQueryList` _ddoDhcpOptionsIds
        , "DryRun"        =? _ddoDryRun
        , "Filter"        `toQueryList` _ddoFilters
        ]

instance ToHeaders DescribeDhcpOptions

instance AWSRequest DescribeDhcpOptions where
    type Sv DescribeDhcpOptions = EC2
    type Rs DescribeDhcpOptions = DescribeDhcpOptionsResponse

    request  = post "DescribeDhcpOptions"
    response = xmlResponse

instance FromXML DescribeDhcpOptionsResponse where
    parseXML x = DescribeDhcpOptionsResponse
        <$> x .@? "dhcpOptionsSet" .!@ mempty
