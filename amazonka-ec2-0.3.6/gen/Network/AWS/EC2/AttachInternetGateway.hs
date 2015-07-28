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

-- Module      : Network.AWS.EC2.AttachInternetGateway
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

-- | Attaches an Internet gateway to a VPC, enabling connectivity between the
-- Internet and the VPC. For more information about your VPC and Internet
-- gateway, see the <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/ Amazon Virtual Private Cloud User Guide>.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AttachInternetGateway.html>
module Network.AWS.EC2.AttachInternetGateway
    (
    -- * Request
      AttachInternetGateway
    -- ** Request constructor
    , attachInternetGateway
    -- ** Request lenses
    , aigDryRun
    , aigInternetGatewayId
    , aigVpcId

    -- * Response
    , AttachInternetGatewayResponse
    -- ** Response constructor
    , attachInternetGatewayResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data AttachInternetGateway = AttachInternetGateway
    { _aigDryRun            :: Maybe Bool
    , _aigInternetGatewayId :: Text
    , _aigVpcId             :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'AttachInternetGateway' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aigDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'aigInternetGatewayId' @::@ 'Text'
--
-- * 'aigVpcId' @::@ 'Text'
--
attachInternetGateway :: Text -- ^ 'aigInternetGatewayId'
                      -> Text -- ^ 'aigVpcId'
                      -> AttachInternetGateway
attachInternetGateway p1 p2 = AttachInternetGateway
    { _aigInternetGatewayId = p1
    , _aigVpcId             = p2
    , _aigDryRun            = Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
aigDryRun :: Lens' AttachInternetGateway (Maybe Bool)
aigDryRun = lens _aigDryRun (\s a -> s { _aigDryRun = a })

-- | The ID of the Internet gateway.
aigInternetGatewayId :: Lens' AttachInternetGateway Text
aigInternetGatewayId =
    lens _aigInternetGatewayId (\s a -> s { _aigInternetGatewayId = a })

-- | The ID of the VPC.
aigVpcId :: Lens' AttachInternetGateway Text
aigVpcId = lens _aigVpcId (\s a -> s { _aigVpcId = a })

data AttachInternetGatewayResponse = AttachInternetGatewayResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'AttachInternetGatewayResponse' constructor.
attachInternetGatewayResponse :: AttachInternetGatewayResponse
attachInternetGatewayResponse = AttachInternetGatewayResponse

instance ToPath AttachInternetGateway where
    toPath = const "/"

instance ToQuery AttachInternetGateway where
    toQuery AttachInternetGateway{..} = mconcat
        [ "DryRun"            =? _aigDryRun
        , "InternetGatewayId" =? _aigInternetGatewayId
        , "VpcId"             =? _aigVpcId
        ]

instance ToHeaders AttachInternetGateway

instance AWSRequest AttachInternetGateway where
    type Sv AttachInternetGateway = EC2
    type Rs AttachInternetGateway = AttachInternetGatewayResponse

    request  = post "AttachInternetGateway"
    response = nullResponse AttachInternetGatewayResponse
