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

-- Module      : Network.AWS.EC2.DeleteVolume
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

-- | Deletes the specified EBS volume. The volume must be in the 'available' state
-- (not attached to an instance).
--
-- The volume may remain in the 'deleting' state for several minutes.
--
-- For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-deleting-volume.html Deleting an Amazon EBS Volume> in the /AmazonElastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteVolume.html>
module Network.AWS.EC2.DeleteVolume
    (
    -- * Request
      DeleteVolume
    -- ** Request constructor
    , deleteVolume
    -- ** Request lenses
    , dv4DryRun
    , dv4VolumeId

    -- * Response
    , DeleteVolumeResponse
    -- ** Response constructor
    , deleteVolumeResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DeleteVolume = DeleteVolume
    { _dv4DryRun   :: Maybe Bool
    , _dv4VolumeId :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DeleteVolume' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dv4DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dv4VolumeId' @::@ 'Text'
--
deleteVolume :: Text -- ^ 'dv4VolumeId'
             -> DeleteVolume
deleteVolume p1 = DeleteVolume
    { _dv4VolumeId = p1
    , _dv4DryRun   = Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
dv4DryRun :: Lens' DeleteVolume (Maybe Bool)
dv4DryRun = lens _dv4DryRun (\s a -> s { _dv4DryRun = a })

-- | The ID of the volume.
dv4VolumeId :: Lens' DeleteVolume Text
dv4VolumeId = lens _dv4VolumeId (\s a -> s { _dv4VolumeId = a })

data DeleteVolumeResponse = DeleteVolumeResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeleteVolumeResponse' constructor.
deleteVolumeResponse :: DeleteVolumeResponse
deleteVolumeResponse = DeleteVolumeResponse

instance ToPath DeleteVolume where
    toPath = const "/"

instance ToQuery DeleteVolume where
    toQuery DeleteVolume{..} = mconcat
        [ "DryRun"   =? _dv4DryRun
        , "VolumeId" =? _dv4VolumeId
        ]

instance ToHeaders DeleteVolume

instance AWSRequest DeleteVolume where
    type Sv DeleteVolume = EC2
    type Rs DeleteVolume = DeleteVolumeResponse

    request  = post "DeleteVolume"
    response = nullResponse DeleteVolumeResponse
