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

-- Module      : Network.AWS.EC2.DetachNetworkInterface
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

-- | Detaches a network interface from an instance.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DetachNetworkInterface.html>
module Network.AWS.EC2.DetachNetworkInterface
    (
    -- * Request
      DetachNetworkInterface
    -- ** Request constructor
    , detachNetworkInterface
    -- ** Request lenses
    , dniAttachmentId
    , dniDryRun
    , dniForce

    -- * Response
    , DetachNetworkInterfaceResponse
    -- ** Response constructor
    , detachNetworkInterfaceResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DetachNetworkInterface = DetachNetworkInterface
    { _dniAttachmentId :: Text
    , _dniDryRun       :: Maybe Bool
    , _dniForce        :: Maybe Bool
    } deriving (Eq, Ord, Read, Show)

-- | 'DetachNetworkInterface' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dniAttachmentId' @::@ 'Text'
--
-- * 'dniDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dniForce' @::@ 'Maybe' 'Bool'
--
detachNetworkInterface :: Text -- ^ 'dniAttachmentId'
                       -> DetachNetworkInterface
detachNetworkInterface p1 = DetachNetworkInterface
    { _dniAttachmentId = p1
    , _dniDryRun       = Nothing
    , _dniForce        = Nothing
    }

-- | The ID of the attachment.
dniAttachmentId :: Lens' DetachNetworkInterface Text
dniAttachmentId = lens _dniAttachmentId (\s a -> s { _dniAttachmentId = a })

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
dniDryRun :: Lens' DetachNetworkInterface (Maybe Bool)
dniDryRun = lens _dniDryRun (\s a -> s { _dniDryRun = a })

-- | Specifies whether to force a detachment.
dniForce :: Lens' DetachNetworkInterface (Maybe Bool)
dniForce = lens _dniForce (\s a -> s { _dniForce = a })

data DetachNetworkInterfaceResponse = DetachNetworkInterfaceResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DetachNetworkInterfaceResponse' constructor.
detachNetworkInterfaceResponse :: DetachNetworkInterfaceResponse
detachNetworkInterfaceResponse = DetachNetworkInterfaceResponse

instance ToPath DetachNetworkInterface where
    toPath = const "/"

instance ToQuery DetachNetworkInterface where
    toQuery DetachNetworkInterface{..} = mconcat
        [ "AttachmentId" =? _dniAttachmentId
        , "DryRun"       =? _dniDryRun
        , "Force"        =? _dniForce
        ]

instance ToHeaders DetachNetworkInterface

instance AWSRequest DetachNetworkInterface where
    type Sv DetachNetworkInterface = EC2
    type Rs DetachNetworkInterface = DetachNetworkInterfaceResponse

    request  = post "DetachNetworkInterface"
    response = nullResponse DetachNetworkInterfaceResponse
