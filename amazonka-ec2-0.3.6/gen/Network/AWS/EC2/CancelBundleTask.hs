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

-- Module      : Network.AWS.EC2.CancelBundleTask
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

-- | Cancels a bundling operation for an instance store-backed Windows instance.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CancelBundleTask.html>
module Network.AWS.EC2.CancelBundleTask
    (
    -- * Request
      CancelBundleTask
    -- ** Request constructor
    , cancelBundleTask
    -- ** Request lenses
    , cbtBundleId
    , cbtDryRun

    -- * Response
    , CancelBundleTaskResponse
    -- ** Response constructor
    , cancelBundleTaskResponse
    -- ** Response lenses
    , cbtrBundleTask
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data CancelBundleTask = CancelBundleTask
    { _cbtBundleId :: Text
    , _cbtDryRun   :: Maybe Bool
    } deriving (Eq, Ord, Read, Show)

-- | 'CancelBundleTask' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cbtBundleId' @::@ 'Text'
--
-- * 'cbtDryRun' @::@ 'Maybe' 'Bool'
--
cancelBundleTask :: Text -- ^ 'cbtBundleId'
                 -> CancelBundleTask
cancelBundleTask p1 = CancelBundleTask
    { _cbtBundleId = p1
    , _cbtDryRun   = Nothing
    }

-- | The ID of the bundle task.
cbtBundleId :: Lens' CancelBundleTask Text
cbtBundleId = lens _cbtBundleId (\s a -> s { _cbtBundleId = a })

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
cbtDryRun :: Lens' CancelBundleTask (Maybe Bool)
cbtDryRun = lens _cbtDryRun (\s a -> s { _cbtDryRun = a })

newtype CancelBundleTaskResponse = CancelBundleTaskResponse
    { _cbtrBundleTask :: Maybe BundleTask
    } deriving (Eq, Read, Show)

-- | 'CancelBundleTaskResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cbtrBundleTask' @::@ 'Maybe' 'BundleTask'
--
cancelBundleTaskResponse :: CancelBundleTaskResponse
cancelBundleTaskResponse = CancelBundleTaskResponse
    { _cbtrBundleTask = Nothing
    }

-- | Information about the bundle task.
cbtrBundleTask :: Lens' CancelBundleTaskResponse (Maybe BundleTask)
cbtrBundleTask = lens _cbtrBundleTask (\s a -> s { _cbtrBundleTask = a })

instance ToPath CancelBundleTask where
    toPath = const "/"

instance ToQuery CancelBundleTask where
    toQuery CancelBundleTask{..} = mconcat
        [ "BundleId" =? _cbtBundleId
        , "DryRun"   =? _cbtDryRun
        ]

instance ToHeaders CancelBundleTask

instance AWSRequest CancelBundleTask where
    type Sv CancelBundleTask = EC2
    type Rs CancelBundleTask = CancelBundleTaskResponse

    request  = post "CancelBundleTask"
    response = xmlResponse

instance FromXML CancelBundleTaskResponse where
    parseXML x = CancelBundleTaskResponse
        <$> x .@? "bundleInstanceTask"
