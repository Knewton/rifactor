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

-- Module      : Network.AWS.EC2.ResetImageAttribute
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

-- | Resets an attribute of an AMI to its default value.
--
-- The productCodes attribute can't be reset.
--
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ResetImageAttribute.html>
module Network.AWS.EC2.ResetImageAttribute
    (
    -- * Request
      ResetImageAttribute
    -- ** Request constructor
    , resetImageAttribute
    -- ** Request lenses
    , ria1Attribute
    , ria1DryRun
    , ria1ImageId

    -- * Response
    , ResetImageAttributeResponse
    -- ** Response constructor
    , resetImageAttributeResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data ResetImageAttribute = ResetImageAttribute
    { _ria1Attribute :: ResetImageAttributeName
    , _ria1DryRun    :: Maybe Bool
    , _ria1ImageId   :: Text
    } deriving (Eq, Read, Show)

-- | 'ResetImageAttribute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ria1Attribute' @::@ 'ResetImageAttributeName'
--
-- * 'ria1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'ria1ImageId' @::@ 'Text'
--
resetImageAttribute :: Text -- ^ 'ria1ImageId'
                    -> ResetImageAttributeName -- ^ 'ria1Attribute'
                    -> ResetImageAttribute
resetImageAttribute p1 p2 = ResetImageAttribute
    { _ria1ImageId   = p1
    , _ria1Attribute = p2
    , _ria1DryRun    = Nothing
    }

-- | The attribute to reset (currently you can only reset the launch permission
-- attribute).
ria1Attribute :: Lens' ResetImageAttribute ResetImageAttributeName
ria1Attribute = lens _ria1Attribute (\s a -> s { _ria1Attribute = a })

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
ria1DryRun :: Lens' ResetImageAttribute (Maybe Bool)
ria1DryRun = lens _ria1DryRun (\s a -> s { _ria1DryRun = a })

-- | The ID of the AMI.
ria1ImageId :: Lens' ResetImageAttribute Text
ria1ImageId = lens _ria1ImageId (\s a -> s { _ria1ImageId = a })

data ResetImageAttributeResponse = ResetImageAttributeResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'ResetImageAttributeResponse' constructor.
resetImageAttributeResponse :: ResetImageAttributeResponse
resetImageAttributeResponse = ResetImageAttributeResponse

instance ToPath ResetImageAttribute where
    toPath = const "/"

instance ToQuery ResetImageAttribute where
    toQuery ResetImageAttribute{..} = mconcat
        [ "Attribute" =? _ria1Attribute
        , "DryRun"    =? _ria1DryRun
        , "ImageId"   =? _ria1ImageId
        ]

instance ToHeaders ResetImageAttribute

instance AWSRequest ResetImageAttribute where
    type Sv ResetImageAttribute = EC2
    type Rs ResetImageAttribute = ResetImageAttributeResponse

    request  = post "ResetImageAttribute"
    response = nullResponse ResetImageAttributeResponse
