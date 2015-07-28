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

-- Module      : Network.AWS.EC2.DeleteSubnet
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

-- | Deletes the specified subnet. You must terminate all running instances in the
-- subnet before you can delete the subnet.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteSubnet.html>
module Network.AWS.EC2.DeleteSubnet
    (
    -- * Request
      DeleteSubnet
    -- ** Request constructor
    , deleteSubnet
    -- ** Request lenses
    , ds2DryRun
    , ds2SubnetId

    -- * Response
    , DeleteSubnetResponse
    -- ** Response constructor
    , deleteSubnetResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DeleteSubnet = DeleteSubnet
    { _ds2DryRun   :: Maybe Bool
    , _ds2SubnetId :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DeleteSubnet' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ds2DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'ds2SubnetId' @::@ 'Text'
--
deleteSubnet :: Text -- ^ 'ds2SubnetId'
             -> DeleteSubnet
deleteSubnet p1 = DeleteSubnet
    { _ds2SubnetId = p1
    , _ds2DryRun   = Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
ds2DryRun :: Lens' DeleteSubnet (Maybe Bool)
ds2DryRun = lens _ds2DryRun (\s a -> s { _ds2DryRun = a })

-- | The ID of the subnet.
ds2SubnetId :: Lens' DeleteSubnet Text
ds2SubnetId = lens _ds2SubnetId (\s a -> s { _ds2SubnetId = a })

data DeleteSubnetResponse = DeleteSubnetResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeleteSubnetResponse' constructor.
deleteSubnetResponse :: DeleteSubnetResponse
deleteSubnetResponse = DeleteSubnetResponse

instance ToPath DeleteSubnet where
    toPath = const "/"

instance ToQuery DeleteSubnet where
    toQuery DeleteSubnet{..} = mconcat
        [ "DryRun"   =? _ds2DryRun
        , "SubnetId" =? _ds2SubnetId
        ]

instance ToHeaders DeleteSubnet

instance AWSRequest DeleteSubnet where
    type Sv DeleteSubnet = EC2
    type Rs DeleteSubnet = DeleteSubnetResponse

    request  = post "DeleteSubnet"
    response = nullResponse DeleteSubnetResponse
