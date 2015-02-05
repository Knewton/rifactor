{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- Module      : Rifactor.Summary
-- Copyright   : (c) 2015 Knewton, Inc <se@knewton.com>
--               (c) 2015 Tim Dysinger <tim@dysinger.net> (contributor)
-- License     : Apache 2.0 http://opensource.org/licenses/Apache-2.0
-- Maintainer  : Tim Dysinger <tim@dysinger.net>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Rifactor.Summary where

import           BasePrelude
import           Control.Lens
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.AWS.Data (toText)
import           Network.AWS.EC2.Types hiding (Region)
import           Rifactor.Capacity
import           Rifactor.Types

{- Classes/Instances -}

-- | This partial function will intersperse commas through any list of
-- Text given it.
sep :: [Text] -> Text
sep = T.intercalate (T.pack " | ")

-- | This class is for summarizing things to print to the console (or
-- a report).  It's pretty simple & straightforward. It only has
-- limited features.
class Summarizable a where
  summary :: a -> Text

-- | A summary of a Model as a whole.
instance Summarizable Model where
  summary (Model rs od cs) =
    "Model " <>
    sep (map summary rs) <>
    " | " <>
    sep (map summary od) <>
    " | " <>
    sep (map summary cs)

-- | A summary of a Reserved recard (many different).
instance Summarizable Reserved where
  summary r@(Reserved _ ri is nis) =
    "Reserved " <>
    (summary ri) <>
    (case capacity r of
       Nothing -> T.empty
       Just (total,used) ->
         " capacity " <>
         T.pack (show used) <>
         " of " <>
         T.pack (show total) <>
         " total") <>
    (if length is > 0
        then " used by " <>
             (T.pack $ show $ length is) <>
             " [" <>
             sep (map summary is) <>
             "]"
        else "") <>
    (if length nis > 0
        then " plans call for " <>
             (T.pack $ show $ length nis) <>
             " more [" <>
             sep (map summary nis) <>
             "]"
        else "")

instance Summarizable Combine where
  summary (Combine rs) =
    "Combine [" <>
    sep (map summary rs) <>
    "]"

-- | A summary of a OnDemand recard.
instance Summarizable OnDemand where
  summary (OnDemand _ i) = summary i

-- | A summary of a ReservedInstances recard.
instance Summarizable ReservedInstances where
  summary x =
    fromMaybe T.empty (x ^. ri1ReservedInstancesId) <>
    "|" <>
    toText (fromMaybe 0 (x ^. ri1InstanceCount)) <>
    "|" <>
    maybe T.empty toText (x ^. ri1InstanceType) <>
    "|" <>
    fromMaybe T.empty (x ^. ri1AvailabilityZone)

-- | A summary of a Instance recard.
instance Summarizable Instance where
  summary x =
    (x ^. i1InstanceId) <>
    "|" <>
    toText (x ^. i1InstanceType) <>
    "|" <>
    maybe T.empty toText (x ^. i1Placement ^. pAvailabilityZone)
