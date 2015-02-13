{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- Module      : Rifactor.Summary
-- Copyright   : (c) 2015 Knewton, Inc <se@knewton.com>
--               (c) 2015 Tim Dysinger <tim@dysinger.net> (contributor)
-- License     : Apache 2.0 http://opensource.org/licenses/Apache-2.0
-- Maintainer  : Tim Dysinger <tim@dysinger.net>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Rifactor.Summary where

-- import           BasePrelude
-- import           Control.Lens
-- import           Data.Text (Text)
-- import qualified Data.Text as T
-- import           Network.AWS.Data (toText)
-- import           Network.AWS.EC2 hiding (Instance)
-- import qualified Network.AWS.EC2 as EC2
-- import           Rifactor.Capacity
-- import           Rifactor.Types

-- default (Text)

-- {- Classes/Instances -}

-- -- | This partial function will intersperse commas through any list of
-- -- Text given it.
-- sep :: [Text] -> Text
-- sep = T.intercalate (" - ")

-- -- | This class is for summarizing things to print to the console (or
-- -- a report).  It's pretty simple & straightforward. It only has
-- -- limited features.
-- class Summarizable a where
--   summary :: a -> Text

-- -- | A summary of a Model as a whole.
-- instance Summarizable Model where
--   summary (Model rs od cs) =
--     "Model " <>
--     sep (map summary rs) <>
--     " - " <>
--     sep (map summary od) <>
--     " - " <>
--     sep (map summary cs)

-- -- | A summary of a Reserved recard (many different).
-- instance Summarizable Reserved where
--   summary r@(Reserved a _ ri is nis) =
--     "Reserved (" <> a <> ") " <>
--     (summary ri) <>
--     (if length is > 0
--         then " applicable " <>
--              (fromString . show $ length is) <>
--              " [" <>
--              sep (map summary is) <>
--              "]"
--         else "") <>
--     (if length nis > 0
--         then " additional [" <>
--              sep (map summary nis) <>
--              "]"
--         else "")
--     -- (case capacity r of
--     --    Nothing -> T.empty
--     --    Just (total,used) ->
--     --      " capacity " <>
--     --      (fromString . show $ used) <>
--     --      " of " <>
--     --      (fromString . show $ total) <>
--     --      " total")

-- -- | A summary of a Instance recard.
-- instance Summarizable Instance where
--   summary (Instance a _ i) = "Instance (" <> a <> ") " <> summary i

-- instance Summarizable Combine where
--   summary (Combine rs) =
--     "Combine [" <>
--     sep (map summary rs) <>
--     "]"

-- -- | A summary of a ReservedInstances recard.
-- instance Summarizable EC2.ReservedInstances where
--   summary x =
--     fromMaybe T.empty (x ^. ri1ReservedInstancesId) <>
--     "|" <>
--     toText (fromMaybe 0 (x ^. ri1InstanceCount)) <>
--     "|" <>
--     maybe T.empty toText (x ^. ri1InstanceType) <>
--     "|" <>
--     fromMaybe T.empty (x ^. ri1AvailabilityZone) <>
--     "|" <>
--     (if ((x ^. ri1ProductDescription) `elem`
--          [Just RIPDWindows,Just RIPDWindowsAmazonVPC])
--      then "vpc"
--      else "classic")

-- -- | A summary of a Instance recard.
-- instance Summarizable EC2.Instance where
--   summary x =
--     (x ^. i1InstanceId) <>
--     "|" <>
--     toText (x ^. i1InstanceType) <>
--     "|" <>
--     maybe T.empty toText (x ^. i1Placement ^. pAvailabilityZone) <>
--     "|" <>
--     maybe "classic" (\_ -> "vpc") (x ^. i1VpcId)
