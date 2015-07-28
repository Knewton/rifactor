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

-- Module      : Network.AWS.EC2.AcceptVpcPeeringConnection
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

-- | Accept a VPC peering connection request. To accept a request, the VPC peering
-- connection must be in the 'pending-acceptance' state, and you must be the owner
-- of the peer VPC. Use the 'DescribeVpcPeeringConnections' request to view your
-- outstanding VPC peering connection requests.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AcceptVpcPeeringConnection.html>
module Network.AWS.EC2.AcceptVpcPeeringConnection
    (
    -- * Request
      AcceptVpcPeeringConnection
    -- ** Request constructor
    , acceptVpcPeeringConnection
    -- ** Request lenses
    , avpcDryRun
    , avpcVpcPeeringConnectionId

    -- * Response
    , AcceptVpcPeeringConnectionResponse
    -- ** Response constructor
    , acceptVpcPeeringConnectionResponse
    -- ** Response lenses
    , avpcrVpcPeeringConnection
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data AcceptVpcPeeringConnection = AcceptVpcPeeringConnection
    { _avpcDryRun                 :: Maybe Bool
    , _avpcVpcPeeringConnectionId :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'AcceptVpcPeeringConnection' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avpcDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'avpcVpcPeeringConnectionId' @::@ 'Maybe' 'Text'
--
acceptVpcPeeringConnection :: AcceptVpcPeeringConnection
acceptVpcPeeringConnection = AcceptVpcPeeringConnection
    { _avpcDryRun                 = Nothing
    , _avpcVpcPeeringConnectionId = Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
avpcDryRun :: Lens' AcceptVpcPeeringConnection (Maybe Bool)
avpcDryRun = lens _avpcDryRun (\s a -> s { _avpcDryRun = a })

-- | The ID of the VPC peering connection.
avpcVpcPeeringConnectionId :: Lens' AcceptVpcPeeringConnection (Maybe Text)
avpcVpcPeeringConnectionId =
    lens _avpcVpcPeeringConnectionId
        (\s a -> s { _avpcVpcPeeringConnectionId = a })

newtype AcceptVpcPeeringConnectionResponse = AcceptVpcPeeringConnectionResponse
    { _avpcrVpcPeeringConnection :: Maybe VpcPeeringConnection
    } deriving (Eq, Read, Show)

-- | 'AcceptVpcPeeringConnectionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avpcrVpcPeeringConnection' @::@ 'Maybe' 'VpcPeeringConnection'
--
acceptVpcPeeringConnectionResponse :: AcceptVpcPeeringConnectionResponse
acceptVpcPeeringConnectionResponse = AcceptVpcPeeringConnectionResponse
    { _avpcrVpcPeeringConnection = Nothing
    }

-- | Information about the VPC peering connection.
avpcrVpcPeeringConnection :: Lens' AcceptVpcPeeringConnectionResponse (Maybe VpcPeeringConnection)
avpcrVpcPeeringConnection =
    lens _avpcrVpcPeeringConnection
        (\s a -> s { _avpcrVpcPeeringConnection = a })

instance ToPath AcceptVpcPeeringConnection where
    toPath = const "/"

instance ToQuery AcceptVpcPeeringConnection where
    toQuery AcceptVpcPeeringConnection{..} = mconcat
        [ "DryRun"                 =? _avpcDryRun
        , "VpcPeeringConnectionId" =? _avpcVpcPeeringConnectionId
        ]

instance ToHeaders AcceptVpcPeeringConnection

instance AWSRequest AcceptVpcPeeringConnection where
    type Sv AcceptVpcPeeringConnection = EC2
    type Rs AcceptVpcPeeringConnection = AcceptVpcPeeringConnectionResponse

    request  = post "AcceptVpcPeeringConnection"
    response = xmlResponse

instance FromXML AcceptVpcPeeringConnectionResponse where
    parseXML x = AcceptVpcPeeringConnectionResponse
        <$> x .@? "vpcPeeringConnection"
