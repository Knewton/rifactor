{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- Module      : Rifactor.Info
-- Copyright   : (c) 2015 Knewton, Inc <se@knewton.com>
--               (c) 2015 Tim Dysinger <tim@dysinger.net> (contributor)
-- License     : Apache 2.0 http://opensource.org/licenses/Apache-2.0
-- Maintainer  : Tim Dysinger <tim@dysinger.net>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Rifactor.Info where

import           BasePrelude
import           Control.Lens hiding ((&))
import qualified Control.Monad.Trans.AWS as AWS
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.AWS.Data (toText)
import           Network.AWS.EC2 hiding (Instance,ReservedInstances)
import qualified Network.AWS.EC2 as EC2

default (Text)

-- info :: forall a. RI a -> Text
-- info (Env n _) = n
-- info (Ins e i) =
--   "Instance[" <> info e <> "|" <>
--   maybe T.empty toText (i ^. i1Placement ^. pAvailabilityZone) <>
--   "|" <>
--   toText (i ^. i1InstanceType) <>
--   "|" <>
--   maybe "classic"
--         (\_ -> "vpc")
--         (i ^. i1VpcId) <>
--   "|" <>
--   (i ^. i1InstanceId) <>
--   "]"
-- info (Res e r is) =
--   "ReservedInstances[" <> info e <> "|" <>
--   fromMaybe T.empty (r ^. ri1AvailabilityZone) <>
--   "|" <>
--   maybe T.empty toText (r ^. ri1InstanceType) <>
--   "|" <>
--   (if ((r ^. ri1ProductDescription) `elem`
--        [Just RIPDWindows,Just RIPDWindowsAmazonVPC])
--       then "vpc"
--       else "classic") <>
--   "|" <>
--   fromMaybe T.empty (r ^. ri1ReservedInstancesId) <>
--   "|" <>
--   toText (fromMaybe 0 (r ^. ri1InstanceCount)) <>
--   "|" <>
--   T.intercalate "+"
--                 (map info is) <>
--   "]"
-- info (Split r is) =
--   "Split[" <> info r <> "|" <>
--   T.intercalate "+"
--                 (map info is) <>
--   "]"
-- info (Merge rs) =
--   "Merge[" <>
--   T.intercalate "+"
--                 (map info rs) <>
--   "]"
-- info (RI is ris mris) =
--   "RI[" <>
--   T.intercalate "\n  "
--                 (map info is) <>
--   "\n\n" <>
--   T.intercalate "\n  "
--                 (map info ris) <>
--   "\n\n" <>
--   T.intercalate "\n  "
--                 (map info mris) <>
--   "\n\n]"
