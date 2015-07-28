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

-- Module      : Network.AWS.EC2.DescribeExportTasks
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

-- | Describes one or more of your export tasks.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeExportTasks.html>
module Network.AWS.EC2.DescribeExportTasks
    (
    -- * Request
      DescribeExportTasks
    -- ** Request constructor
    , describeExportTasks
    -- ** Request lenses
    , detExportTaskIds

    -- * Response
    , DescribeExportTasksResponse
    -- ** Response constructor
    , describeExportTasksResponse
    -- ** Response lenses
    , detrExportTasks
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

newtype DescribeExportTasks = DescribeExportTasks
    { _detExportTaskIds :: List "ExportTaskId" Text
    } deriving (Eq, Ord, Read, Show, Monoid, Semigroup)

-- | 'DescribeExportTasks' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'detExportTaskIds' @::@ ['Text']
--
describeExportTasks :: DescribeExportTasks
describeExportTasks = DescribeExportTasks
    { _detExportTaskIds = mempty
    }

-- | One or more export task IDs.
detExportTaskIds :: Lens' DescribeExportTasks [Text]
detExportTaskIds = lens _detExportTaskIds (\s a -> s { _detExportTaskIds = a }) . _List

newtype DescribeExportTasksResponse = DescribeExportTasksResponse
    { _detrExportTasks :: List "item" ExportTask
    } deriving (Eq, Read, Show, Monoid, Semigroup)

-- | 'DescribeExportTasksResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'detrExportTasks' @::@ ['ExportTask']
--
describeExportTasksResponse :: DescribeExportTasksResponse
describeExportTasksResponse = DescribeExportTasksResponse
    { _detrExportTasks = mempty
    }

-- | Information about the export tasks.
detrExportTasks :: Lens' DescribeExportTasksResponse [ExportTask]
detrExportTasks = lens _detrExportTasks (\s a -> s { _detrExportTasks = a }) . _List

instance ToPath DescribeExportTasks where
    toPath = const "/"

instance ToQuery DescribeExportTasks where
    toQuery DescribeExportTasks{..} = mconcat
        [ "ExportTaskId" `toQueryList` _detExportTaskIds
        ]

instance ToHeaders DescribeExportTasks

instance AWSRequest DescribeExportTasks where
    type Sv DescribeExportTasks = EC2
    type Rs DescribeExportTasks = DescribeExportTasksResponse

    request  = post "DescribeExportTasks"
    response = xmlResponse

instance FromXML DescribeExportTasksResponse where
    parseXML x = DescribeExportTasksResponse
        <$> x .@? "exportTaskSet" .!@ mempty
