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

-- Module      : Network.AWS.EC2.DescribeSpotDatafeedSubscription
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

-- | Describes the data feed for Spot Instances. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-data-feeds.html SpotInstance Data Feed> in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSpotDatafeedSubscription.html>
module Network.AWS.EC2.DescribeSpotDatafeedSubscription
    (
    -- * Request
      DescribeSpotDatafeedSubscription
    -- ** Request constructor
    , describeSpotDatafeedSubscription
    -- ** Request lenses
    , dsdsDryRun

    -- * Response
    , DescribeSpotDatafeedSubscriptionResponse
    -- ** Response constructor
    , describeSpotDatafeedSubscriptionResponse
    -- ** Response lenses
    , dsdsrSpotDatafeedSubscription
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

newtype DescribeSpotDatafeedSubscription = DescribeSpotDatafeedSubscription
    { _dsdsDryRun :: Maybe Bool
    } deriving (Eq, Ord, Read, Show)

-- | 'DescribeSpotDatafeedSubscription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsdsDryRun' @::@ 'Maybe' 'Bool'
--
describeSpotDatafeedSubscription :: DescribeSpotDatafeedSubscription
describeSpotDatafeedSubscription = DescribeSpotDatafeedSubscription
    { _dsdsDryRun = Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
dsdsDryRun :: Lens' DescribeSpotDatafeedSubscription (Maybe Bool)
dsdsDryRun = lens _dsdsDryRun (\s a -> s { _dsdsDryRun = a })

newtype DescribeSpotDatafeedSubscriptionResponse = DescribeSpotDatafeedSubscriptionResponse
    { _dsdsrSpotDatafeedSubscription :: Maybe SpotDatafeedSubscription
    } deriving (Eq, Read, Show)

-- | 'DescribeSpotDatafeedSubscriptionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsdsrSpotDatafeedSubscription' @::@ 'Maybe' 'SpotDatafeedSubscription'
--
describeSpotDatafeedSubscriptionResponse :: DescribeSpotDatafeedSubscriptionResponse
describeSpotDatafeedSubscriptionResponse = DescribeSpotDatafeedSubscriptionResponse
    { _dsdsrSpotDatafeedSubscription = Nothing
    }

-- | The Spot Instance data feed subscription.
dsdsrSpotDatafeedSubscription :: Lens' DescribeSpotDatafeedSubscriptionResponse (Maybe SpotDatafeedSubscription)
dsdsrSpotDatafeedSubscription =
    lens _dsdsrSpotDatafeedSubscription
        (\s a -> s { _dsdsrSpotDatafeedSubscription = a })

instance ToPath DescribeSpotDatafeedSubscription where
    toPath = const "/"

instance ToQuery DescribeSpotDatafeedSubscription where
    toQuery DescribeSpotDatafeedSubscription{..} = mconcat
        [ "DryRun" =? _dsdsDryRun
        ]

instance ToHeaders DescribeSpotDatafeedSubscription

instance AWSRequest DescribeSpotDatafeedSubscription where
    type Sv DescribeSpotDatafeedSubscription = EC2
    type Rs DescribeSpotDatafeedSubscription = DescribeSpotDatafeedSubscriptionResponse

    request  = post "DescribeSpotDatafeedSubscription"
    response = xmlResponse

instance FromXML DescribeSpotDatafeedSubscriptionResponse where
    parseXML x = DescribeSpotDatafeedSubscriptionResponse
        <$> x .@? "spotDatafeedSubscription"
