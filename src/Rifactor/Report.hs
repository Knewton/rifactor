{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- Module      : Rifactor.Report
-- Copyright   : (c) 2015 Knewton, Inc <se@knewton.com>
--               (c) 2015 Tim Dysinger <tim@dysinger.net> (contributor)
-- License     : Apache 2.0 http://opensource.org/licenses/Apache-2.0
-- Maintainer  : Tim Dysinger <tim@dysinger.net>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Rifactor.Report where

import           BasePrelude hiding ((<>),(<+>))
import           Control.Lens
import qualified Control.Monad.Trans.AWS as AWS
import           Control.Monad.Trans.AWS hiding (Env)
import           Data.Text (Text)
import           Data.String
import qualified Data.Text as T
import           Text.PrettyPrint.ANSI.Leijen
import qualified Network.AWS.Data as AWS
import qualified Network.AWS.EC2 as EC2
import           Network.AWS.EC2 hiding (Error,Instance,Region)
import           Rifactor.AWS
import           Rifactor.Types

default (Text)

-- | This class is for summarizing things to print.
class Report a where
  report :: a -> Doc

instance Report AwsEnv where
  report (Env _ b) = toDoc b

instance Report AwsResource where
  report (Reserved a b) = report a <+> report b
  report (Instance a b) = report a <+> report b

instance Report AwsPlan where
  report (None) = red "Done"
  report (Item a) = report a
  report p@(Used a bs) =
    dullgreen "Used" <+>
    "(" <>
    toDoc (T.pack (show (instanceNormFactor p))) <+>
    dullgreen "used of" <+>
    toDoc (T.pack (case (rInstanceNormFactor p) of
                     Just x -> show x
                     Nothing -> "?")) <>
    ")" <>
    linebreak <>
    hang 4 (indent 4 (report a)) <>
    linebreak <>
    dullgreen (indent 2 "used by") <>
    linebreak <>
    hang 4 (indent 4 (vsep (map report bs)))
  report p@(Split a bs) =
    dullmagenta "Split" <+>
    "(" <>
    toDoc (T.pack (show (instanceNormFactor p))) <+>
    dullmagenta "used of" <+>
    toDoc (T.pack (case (rInstanceNormFactor p) of
                     Just x -> show x
                     Nothing -> "?")) <>
    ")" <>
    linebreak <>
    hang 4 (indent 4 (report a)) <>
    linebreak <>
    dullmagenta (indent 2 "by adding") <>
    linebreak <>
    hang 4 (indent 4 (vsep (map report bs)))
  report (Merge as) =
    dullcyan "Merge" <>
    linebreak <>
    hang 4 (indent 4 (vsep (map report as)))
  report (Plans as) =
    vsep (map (\a' -> report a' <> linebreak) as)
    -- TODO group r&i data by region->zone, group->type, vpc/classic
      -- TODO print summary of reserved totals
      -- TODO print summary of instances totals

instance Report EC2.ReservedInstances where
  report x =
    "reserved-instances " <>
    (case (x ^. riReservedInstancesId) of
       Just i ->
         toDoc (T.take 7 (AWS.toText i)) <>
         "|"
       Nothing -> "") <>
    (case fmap find1ByType (x ^. riInstanceType) of
       Just i ->
         toDoc (T.toLower (T.pack (show (i ^. insGroup)))) <>
         "|"
       Nothing -> "") <>
    (if ((x ^. riProductDescription) `elem`
         [Just RIDWindows,Just RIDWindowsAmazonVPC])
        then "vpc"
        else "classic") <>
    "|" <>
    (case (x ^. riAvailabilityZone) of
       Just az -> toDoc az
       Nothing -> "")

instance Report EC2.Instance where
  report x =
    "instance " <>
    toDoc (x ^. insInstanceId) <>
    "|" <>
    toDoc (T.toLower (AWS.toText (x ^. insInstanceType))) <>
    "|" <>
    maybe "classic"
          (\_ -> "vpc")
          (x ^. insVPCId) <>
    "|" <>
    (case x ^. insPlacement ^. pAvailabilityZone of
       Just az -> toDoc az
       Nothing -> "")

toDoc :: Text -> Doc
toDoc = text . T.unpack

-- printGroupSums :: Plan -> IO ()
-- printGroupSums m =
--   let instanceGroups =
--         map (\gs@(i:_) ->
--                (show (i ^. inInstance ^. i1InstanceType)
--                ,(case (i ^. inInstance ^. i1Placement ^. pAvailabilityZone) of
--                    Nothing -> ""
--                    Just az -> T.unpack az)
--                ,"instance"
--                ,if isJust (i ^. inInstance ^. i1VpcId)
--                    then "vpc"
--                    else "classic"
--                ,i ^. inAccount
--                ,(length gs)))
--             (groupBy matchingInstance (sortBy comparingInstance (m ^. instances)))
--       reservedGroups =
--         map (\gs@(r:_) ->
--                (case (r ^. reReserved ^. ri1InstanceType) of
--                   Nothing -> "(unknown)"
--                   Just rType -> show rType
--                ,case (r ^. reReserved ^. ri1AvailabilityZone) of
--                   Nothing -> "(unknown)"
--                   Just az -> T.unpack az
--                ,"reserved"
--                ,if ((r ^. reReserved ^. ri1ProductDescription) `elem`
--                     [Just RIPDLinuxUNIXAmazonVPC,Just RIPDWindowsAmazonVPC])
--                    then "vpc"
--                    else "classic"
--                ,r ^. reAccount
--                ,sum (catMaybes (map (view ri1InstanceCount . view reReserved) gs))))
--             (groupBy matchingReserved
--                      (sortBy comparingReserved (m ^. reserved)))
--   in traverse_ print (sort (concat [instanceGroups,reservedGroups]))
