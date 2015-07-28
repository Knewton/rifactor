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

-- Module      : Network.AWS.EC2.ReplaceNetworkAclAssociation
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

-- | Changes which network ACL a subnet is associated with. By default when you
-- create a subnet, it's automatically associated with the default network ACL.
-- For more information about network ACLs, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_ACLs.html Network ACLs> in the /AmazonVirtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ReplaceNetworkAclAssociation.html>
module Network.AWS.EC2.ReplaceNetworkAclAssociation
    (
    -- * Request
      ReplaceNetworkAclAssociation
    -- ** Request constructor
    , replaceNetworkAclAssociation
    -- ** Request lenses
    , rnaaAssociationId
    , rnaaDryRun
    , rnaaNetworkAclId

    -- * Response
    , ReplaceNetworkAclAssociationResponse
    -- ** Response constructor
    , replaceNetworkAclAssociationResponse
    -- ** Response lenses
    , rnaarNewAssociationId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data ReplaceNetworkAclAssociation = ReplaceNetworkAclAssociation
    { _rnaaAssociationId :: Text
    , _rnaaDryRun        :: Maybe Bool
    , _rnaaNetworkAclId  :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ReplaceNetworkAclAssociation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rnaaAssociationId' @::@ 'Text'
--
-- * 'rnaaDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'rnaaNetworkAclId' @::@ 'Text'
--
replaceNetworkAclAssociation :: Text -- ^ 'rnaaAssociationId'
                             -> Text -- ^ 'rnaaNetworkAclId'
                             -> ReplaceNetworkAclAssociation
replaceNetworkAclAssociation p1 p2 = ReplaceNetworkAclAssociation
    { _rnaaAssociationId = p1
    , _rnaaNetworkAclId  = p2
    , _rnaaDryRun        = Nothing
    }

-- | The ID of the current association between the original network ACL and the
-- subnet.
rnaaAssociationId :: Lens' ReplaceNetworkAclAssociation Text
rnaaAssociationId =
    lens _rnaaAssociationId (\s a -> s { _rnaaAssociationId = a })

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
rnaaDryRun :: Lens' ReplaceNetworkAclAssociation (Maybe Bool)
rnaaDryRun = lens _rnaaDryRun (\s a -> s { _rnaaDryRun = a })

-- | The ID of the new network ACL to associate with the subnet.
rnaaNetworkAclId :: Lens' ReplaceNetworkAclAssociation Text
rnaaNetworkAclId = lens _rnaaNetworkAclId (\s a -> s { _rnaaNetworkAclId = a })

newtype ReplaceNetworkAclAssociationResponse = ReplaceNetworkAclAssociationResponse
    { _rnaarNewAssociationId :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'ReplaceNetworkAclAssociationResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rnaarNewAssociationId' @::@ 'Maybe' 'Text'
--
replaceNetworkAclAssociationResponse :: ReplaceNetworkAclAssociationResponse
replaceNetworkAclAssociationResponse = ReplaceNetworkAclAssociationResponse
    { _rnaarNewAssociationId = Nothing
    }

-- | The ID of the new association.
rnaarNewAssociationId :: Lens' ReplaceNetworkAclAssociationResponse (Maybe Text)
rnaarNewAssociationId =
    lens _rnaarNewAssociationId (\s a -> s { _rnaarNewAssociationId = a })

instance ToPath ReplaceNetworkAclAssociation where
    toPath = const "/"

instance ToQuery ReplaceNetworkAclAssociation where
    toQuery ReplaceNetworkAclAssociation{..} = mconcat
        [ "AssociationId" =? _rnaaAssociationId
        , "DryRun"        =? _rnaaDryRun
        , "NetworkAclId"  =? _rnaaNetworkAclId
        ]

instance ToHeaders ReplaceNetworkAclAssociation

instance AWSRequest ReplaceNetworkAclAssociation where
    type Sv ReplaceNetworkAclAssociation = EC2
    type Rs ReplaceNetworkAclAssociation = ReplaceNetworkAclAssociationResponse

    request  = post "ReplaceNetworkAclAssociation"
    response = xmlResponse

instance FromXML ReplaceNetworkAclAssociationResponse where
    parseXML x = ReplaceNetworkAclAssociationResponse
        <$> x .@? "newAssociationId"
