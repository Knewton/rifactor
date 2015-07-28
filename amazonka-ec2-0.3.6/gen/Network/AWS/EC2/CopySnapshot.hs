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

-- Module      : Network.AWS.EC2.CopySnapshot
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

-- | Copies a point-in-time snapshot of an EBS volume and stores it in Amazon S3.
-- You can copy the snapshot within the same region or from one region to
-- another. You can use the snapshot to create EBS volumes or Amazon Machine
-- Images (AMIs). The snapshot is copied to the regional endpoint that you send
-- the HTTP request to.
--
-- Copies of encrypted EBS snapshots remain encrypted. Copies of unencrypted
-- snapshots remain unencrypted.
--
-- Copying snapshots that were encrypted with non-default AWS Key Management
-- Service (KMS) master keys is not supported at this time.
--
-- For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-copy-snapshot.html Copying an Amazon EBS Snapshot> in the /AmazonElastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CopySnapshot.html>
module Network.AWS.EC2.CopySnapshot
    (
    -- * Request
      CopySnapshot
    -- ** Request constructor
    , copySnapshot
    -- ** Request lenses
    , csDescription
    , csDestinationRegion
    , csDryRun
    , csPresignedUrl
    , csSourceRegion
    , csSourceSnapshotId

    -- * Response
    , CopySnapshotResponse
    -- ** Response constructor
    , copySnapshotResponse
    -- ** Response lenses
    , csrSnapshotId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data CopySnapshot = CopySnapshot
    { _csDescription       :: Maybe Text
    , _csDestinationRegion :: Maybe Text
    , _csDryRun            :: Maybe Bool
    , _csPresignedUrl      :: Maybe Text
    , _csSourceRegion      :: Text
    , _csSourceSnapshotId  :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'CopySnapshot' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csDescription' @::@ 'Maybe' 'Text'
--
-- * 'csDestinationRegion' @::@ 'Maybe' 'Text'
--
-- * 'csDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'csPresignedUrl' @::@ 'Maybe' 'Text'
--
-- * 'csSourceRegion' @::@ 'Text'
--
-- * 'csSourceSnapshotId' @::@ 'Text'
--
copySnapshot :: Text -- ^ 'csSourceRegion'
             -> Text -- ^ 'csSourceSnapshotId'
             -> CopySnapshot
copySnapshot p1 p2 = CopySnapshot
    { _csSourceRegion      = p1
    , _csSourceSnapshotId  = p2
    , _csDryRun            = Nothing
    , _csDescription       = Nothing
    , _csDestinationRegion = Nothing
    , _csPresignedUrl      = Nothing
    }

-- | A description for the EBS snapshot.
csDescription :: Lens' CopySnapshot (Maybe Text)
csDescription = lens _csDescription (\s a -> s { _csDescription = a })

-- | The destination region to use in the 'PresignedUrl' parameter of a snapshot
-- copy operation. This parameter is only valid for specifying the destination
-- region in a 'PresignedUrl' parameter, where it is required.
--
-- 'CopySnapshot' sends the snapshot copy to the regional endpoint that you send
-- the HTTP request to, such as 'ec2.us-east-1.amazonaws.com' (in the AWS CLI,
-- this is specified with the '--region' parameter or the default region in your
-- AWS configuration file).
--
--
csDestinationRegion :: Lens' CopySnapshot (Maybe Text)
csDestinationRegion =
    lens _csDestinationRegion (\s a -> s { _csDestinationRegion = a })

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
csDryRun :: Lens' CopySnapshot (Maybe Bool)
csDryRun = lens _csDryRun (\s a -> s { _csDryRun = a })

-- | The pre-signed URL that facilitates copying an encrypted snapshot. This
-- parameter is only required when copying an encrypted snapshot with the Amazon
-- EC2 Query API; it is available as an optional parameter in all other cases.
-- The 'PresignedUrl' should use the snapshot source endpoint, the 'CopySnapshot'
-- action, and include the 'SourceRegion', 'SourceSnapshotId', and 'DestinationRegion'
-- parameters. The 'PresignedUrl' must be signed using AWS Signature Version 4.
-- Because EBS snapshots are stored in Amazon S3, the signing algorithm for this
-- parameter uses the same logic that is described in <http://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests byUsing Query Parameters (AWS Signature Version 4)> in the /Amazon Simple StorageService API Reference/. An invalid or improperly signed 'PresignedUrl' will
-- cause the copy operation to fail asynchronously, and the snapshot will move
-- to an 'error' state.
csPresignedUrl :: Lens' CopySnapshot (Maybe Text)
csPresignedUrl = lens _csPresignedUrl (\s a -> s { _csPresignedUrl = a })

-- | The ID of the region that contains the snapshot to be copied.
csSourceRegion :: Lens' CopySnapshot Text
csSourceRegion = lens _csSourceRegion (\s a -> s { _csSourceRegion = a })

-- | The ID of the EBS snapshot to copy.
csSourceSnapshotId :: Lens' CopySnapshot Text
csSourceSnapshotId =
    lens _csSourceSnapshotId (\s a -> s { _csSourceSnapshotId = a })

newtype CopySnapshotResponse = CopySnapshotResponse
    { _csrSnapshotId :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'CopySnapshotResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csrSnapshotId' @::@ 'Maybe' 'Text'
--
copySnapshotResponse :: CopySnapshotResponse
copySnapshotResponse = CopySnapshotResponse
    { _csrSnapshotId = Nothing
    }

-- | The ID of the new snapshot.
csrSnapshotId :: Lens' CopySnapshotResponse (Maybe Text)
csrSnapshotId = lens _csrSnapshotId (\s a -> s { _csrSnapshotId = a })

instance ToPath CopySnapshot where
    toPath = const "/"

instance ToQuery CopySnapshot where
    toQuery CopySnapshot{..} = mconcat
        [ "Description"       =? _csDescription
        , "DestinationRegion" =? _csDestinationRegion
        , "DryRun"            =? _csDryRun
        , "PresignedUrl"      =? _csPresignedUrl
        , "SourceRegion"      =? _csSourceRegion
        , "SourceSnapshotId"  =? _csSourceSnapshotId
        ]

instance ToHeaders CopySnapshot

instance AWSRequest CopySnapshot where
    type Sv CopySnapshot = EC2
    type Rs CopySnapshot = CopySnapshotResponse

    request  = post "CopySnapshot"
    response = xmlResponse

instance FromXML CopySnapshotResponse where
    parseXML x = CopySnapshotResponse
        <$> x .@? "snapshotId"
