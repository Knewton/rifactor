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

-- Module      : Network.AWS.EC2.CreateVpnConnectionRoute
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

-- | Creates a static route associated with a VPN connection between an existing
-- virtual private gateway and a VPN customer gateway. The static route allows
-- traffic to be routed from the virtual private gateway to the VPN customer
-- gateway.
--
-- For more information about VPN connections, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_VPN.html Adding a Hardware VirtualPrivate Gateway to Your VPC> in the /Amazon Virtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVpnConnectionRoute.html>
module Network.AWS.EC2.CreateVpnConnectionRoute
    (
    -- * Request
      CreateVpnConnectionRoute
    -- ** Request constructor
    , createVpnConnectionRoute
    -- ** Request lenses
    , cvcrDestinationCidrBlock
    , cvcrVpnConnectionId

    -- * Response
    , CreateVpnConnectionRouteResponse
    -- ** Response constructor
    , createVpnConnectionRouteResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data CreateVpnConnectionRoute = CreateVpnConnectionRoute
    { _cvcrDestinationCidrBlock :: Text
    , _cvcrVpnConnectionId      :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'CreateVpnConnectionRoute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvcrDestinationCidrBlock' @::@ 'Text'
--
-- * 'cvcrVpnConnectionId' @::@ 'Text'
--
createVpnConnectionRoute :: Text -- ^ 'cvcrVpnConnectionId'
                         -> Text -- ^ 'cvcrDestinationCidrBlock'
                         -> CreateVpnConnectionRoute
createVpnConnectionRoute p1 p2 = CreateVpnConnectionRoute
    { _cvcrVpnConnectionId      = p1
    , _cvcrDestinationCidrBlock = p2
    }

-- | The CIDR block associated with the local subnet of the customer network.
cvcrDestinationCidrBlock :: Lens' CreateVpnConnectionRoute Text
cvcrDestinationCidrBlock =
    lens _cvcrDestinationCidrBlock
        (\s a -> s { _cvcrDestinationCidrBlock = a })

-- | The ID of the VPN connection.
cvcrVpnConnectionId :: Lens' CreateVpnConnectionRoute Text
cvcrVpnConnectionId =
    lens _cvcrVpnConnectionId (\s a -> s { _cvcrVpnConnectionId = a })

data CreateVpnConnectionRouteResponse = CreateVpnConnectionRouteResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'CreateVpnConnectionRouteResponse' constructor.
createVpnConnectionRouteResponse :: CreateVpnConnectionRouteResponse
createVpnConnectionRouteResponse = CreateVpnConnectionRouteResponse

instance ToPath CreateVpnConnectionRoute where
    toPath = const "/"

instance ToQuery CreateVpnConnectionRoute where
    toQuery CreateVpnConnectionRoute{..} = mconcat
        [ "DestinationCidrBlock" =? _cvcrDestinationCidrBlock
        , "VpnConnectionId"      =? _cvcrVpnConnectionId
        ]

instance ToHeaders CreateVpnConnectionRoute

instance AWSRequest CreateVpnConnectionRoute where
    type Sv CreateVpnConnectionRoute = EC2
    type Rs CreateVpnConnectionRoute = CreateVpnConnectionRouteResponse

    request  = post "CreateVpnConnectionRoute"
    response = nullResponse CreateVpnConnectionRouteResponse
