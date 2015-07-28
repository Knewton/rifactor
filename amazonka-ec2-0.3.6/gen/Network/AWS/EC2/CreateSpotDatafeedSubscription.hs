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

-- Module      : Network.AWS.EC2.CreateSpotDatafeedSubscription
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

-- | Creates a data feed for Spot Instances, enabling you to view Spot Instance
-- usage logs. You can create one data feed per AWS account. For more
-- information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-data-feeds.html Spot Instance Data Feed> in the /Amazon Elastic Compute CloudUser Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateSpotDatafeedSubscription.html>
module Network.AWS.EC2.CreateSpotDatafeedSubscription
    (
    -- * Request
      CreateSpotDatafeedSubscription
    -- ** Request constructor
    , createSpotDatafeedSubscription
    -- ** Request lenses
    , csdsBucket
    , csdsDryRun
    , csdsPrefix

    -- * Response
    , CreateSpotDatafeedSubscriptionResponse
    -- ** Response constructor
    , createSpotDatafeedSubscriptionResponse
    -- ** Response lenses
    , csdsrSpotDatafeedSubscription
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data CreateSpotDatafeedSubscription = CreateSpotDatafeedSubscription
    { _csdsBucket :: Text
    , _csdsDryRun :: Maybe Bool
    , _csdsPrefix :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'CreateSpotDatafeedSubscription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csdsBucket' @::@ 'Text'
--
-- * 'csdsDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'csdsPrefix' @::@ 'Maybe' 'Text'
--
createSpotDatafeedSubscription :: Text -- ^ 'csdsBucket'
                               -> CreateSpotDatafeedSubscription
createSpotDatafeedSubscription p1 = CreateSpotDatafeedSubscription
    { _csdsBucket = p1
    , _csdsDryRun = Nothing
    , _csdsPrefix = Nothing
    }

-- | The Amazon S3 bucket in which to store the Spot Instance data feed.
csdsBucket :: Lens' CreateSpotDatafeedSubscription Text
csdsBucket = lens _csdsBucket (\s a -> s { _csdsBucket = a })

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
csdsDryRun :: Lens' CreateSpotDatafeedSubscription (Maybe Bool)
csdsDryRun = lens _csdsDryRun (\s a -> s { _csdsDryRun = a })

-- | A prefix for the data feed file names.
csdsPrefix :: Lens' CreateSpotDatafeedSubscription (Maybe Text)
csdsPrefix = lens _csdsPrefix (\s a -> s { _csdsPrefix = a })

newtype CreateSpotDatafeedSubscriptionResponse = CreateSpotDatafeedSubscriptionResponse
    { _csdsrSpotDatafeedSubscription :: Maybe SpotDatafeedSubscription
    } deriving (Eq, Read, Show)

-- | 'CreateSpotDatafeedSubscriptionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csdsrSpotDatafeedSubscription' @::@ 'Maybe' 'SpotDatafeedSubscription'
--
createSpotDatafeedSubscriptionResponse :: CreateSpotDatafeedSubscriptionResponse
createSpotDatafeedSubscriptionResponse = CreateSpotDatafeedSubscriptionResponse
    { _csdsrSpotDatafeedSubscription = Nothing
    }

-- | The Spot Instance data feed subscription.
csdsrSpotDatafeedSubscription :: Lens' CreateSpotDatafeedSubscriptionResponse (Maybe SpotDatafeedSubscription)
csdsrSpotDatafeedSubscription =
    lens _csdsrSpotDatafeedSubscription
        (\s a -> s { _csdsrSpotDatafeedSubscription = a })

instance ToPath CreateSpotDatafeedSubscription where
    toPath = const "/"

instance ToQuery CreateSpotDatafeedSubscription where
    toQuery CreateSpotDatafeedSubscription{..} = mconcat
        [ "Bucket" =? _csdsBucket
        , "DryRun" =? _csdsDryRun
        , "Prefix" =? _csdsPrefix
        ]

instance ToHeaders CreateSpotDatafeedSubscription

instance AWSRequest CreateSpotDatafeedSubscription where
    type Sv CreateSpotDatafeedSubscription = EC2
    type Rs CreateSpotDatafeedSubscription = CreateSpotDatafeedSubscriptionResponse

    request  = post "CreateSpotDatafeedSubscription"
    response = xmlResponse

instance FromXML CreateSpotDatafeedSubscriptionResponse where
    parseXML x = CreateSpotDatafeedSubscriptionResponse
        <$> x .@? "spotDatafeedSubscription"
