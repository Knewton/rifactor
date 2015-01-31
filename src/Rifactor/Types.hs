{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- Module      : Rifactor.Types
-- Copyright   : (c) 2015 Knewton, Inc <se@knewton.com>
--               (c) 2015 Tim Dysinger <tim@dysinger.net> (contributor)
-- License     : Apache 2.0 http://opensource.org/licenses/Apache-2.0
-- Maintainer  : Tim Dysinger <tim@dysinger.net>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Rifactor.Types where

import           BasePrelude
import           Control.Lens
import           Data.Aeson.TH (deriveJSON)
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.AWS
import           Network.AWS.Data (toText)
import           Network.AWS.EC2.Types hiding (Region)
import           Rifactor.Types.Internal (deriveOptions)

-- | Our runtime Options for planning any EC2 Reserved Instance
-- movement.  We need a JSON file path with the configuration in it,
-- we need to know whether to do a dry run or not and finally we need
-- to know if we should be verbose & spit out all the Amazon API call
-- data & rq/rs information.
data Options =
  Options {_file :: FilePath
          ,_dry :: Bool
          ,_verbose :: Bool }

-- | A Record to represet an Account (Amazon Account). This record
-- type translates to/from JSON in the config file.  It gives us our
-- IAM credentials with which to run.
data Account =
  Account {_name :: String
          ,_accessKey :: String
          ,_secretKey :: String}

-- | Config is a combination of accounts & regions we want to cover
-- for all the accounts.  It looks like so:
-- {
--   "accounts": [
--     {
--       "access_key": "AKIAIAD9VZRWT1NS2A7Q",
--       "secret_key": "/0wyT2f1cE3d9mcRWokc2avHqaQY79onabdgZqDv",
--       "name": "production"
--     },
--     {
--       "access_key": "AKIAJV7QP3BA6NHTZJCQ",
--       "secret_key": "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",
--       "name": "staging"
--     },
--     {
--       "access_key": "AKIAJMLRD3JB74IJIN7A",
--       "secret_key": "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",
--       "name": "development"
--     }
--   ],
--   "regions": [
--     "NorthCalifornia",
--     "NorthVirginia",
--     "Oregon"
--   ]
-- }
data Config =
  Config {_accounts :: [Account]
         ,_regions :: [Region]}

-- | We need an instance of Eq so our derived instance of Eq for
-- Reserved & OnDemand will work.  Nevermind "orphaned instance"
-- errors. We need this or else we have to implement our Eq instances
-- for our Model by hand.
instance Eq Env

-- | OnDemand represents Instances that we haven't found a match (with
-- ReservedInstances) yet.  We can also track the Env that we got the
-- instance from in the record.
data OnDemand =
  OnDemand {_odEnv :: Env
           ,_odInstance :: Instance}
  deriving (Eq)

-- | Reserved represents ReservedInstances that are going through
-- transitions as we figure out what they are & what to do with them.
-- Reserved starts out as Reserved (Unknown/Unused) and through a
-- series of Transition functions, comes out the other end as
-- CombineReserved, SplitReserved, etc.
data Reserved
  = Reserved {_reEnv :: Env
             ,_reReservedInstances :: ReservedInstances}
  | UsedReserved {_reEnv :: Env
                 ,_reReservedInstances :: ReservedInstances
                 ,_reInstances :: [Instance]}
  | MoveReserved {_reEnv :: Env
                 ,_reReservedInstances :: ReservedInstances
                 ,_reNewInstances :: [Instance]}
  | SplitReserved {_reEnv :: Env
                  ,_reReservedInstances :: ReservedInstances
                  ,_reInstances :: [Instance]
                  ,_reNewInstances :: [Instance]}
  | CombineReserved {_reEnv :: Env
                    ,_reReservedInstances' :: [ReservedInstances]}
  | ResizeReserved {_reEnv :: Env
                   ,_reReservedInstances :: ReservedInstances
                   ,_reInstances :: [Instance]}
  deriving (Eq)

-- | This is our (simple) data model.  We have a list of Reserved
-- instances and we have a list of OnDemand instances.  We try to
-- match them together as we run through our algorithms.
type Model = ([Reserved],[OnDemand])

-- | A Transition represents a state transition between two models.
-- It is the type signature of several functions that transform the
-- Model.
type Transition = Model -> Model

-- | This is hear just for fun as a unicode alias to "error" (used
-- when we want to bail from an unrecoverable error condition.)
ಠ_ಠ :: String -> a
ಠ_ಠ = error

{-
Lens: These declarations are here to generate code for us.  They
generate "lens" which gives us really powerful ways of view & updating
our records.
-}

$(makeLenses ''Account)
$(makeLenses ''Config)
$(makeLenses ''OnDemand)
$(makeLenses ''Options)
$(makeLenses ''Reserved)

{-
JSON: These declarations are here to generate code for us.  They
generate JSON code to translate our record types to and from JSON.
See Rifactor.Types.Internal.deriveOptions on how we translate from our
record's camel case to snake case (which looks better in JSON files on
disk.)
-}

$(deriveJSON deriveOptions ''Account)
$(deriveJSON deriveOptions ''Config)
$(deriveJSON deriveOptions ''Options)
$(deriveJSON deriveOptions ''Region) -- We even add a JSON instance
                                     -- for a data type from the
                                     -- Amazonka libs (Region).

{- Classes/Instances -}

-- | This partial function will intersperse commas through any list of
-- Text given it.
commaSep :: [Text] -> Text
commaSep = T.intercalate (T.pack ", ")

-- | This class is for summarizing things to print to the console (or
-- a report).  It's pretty simple & straightforward. It only has
-- limited features.
class Summarizable a where
  summary :: a -> Text

-- | A summary of a Model as a whole.
instance Summarizable Model where
  summary (rs,od) =
    "Model " <>
    commaSep (map summary rs) <>
    " " <>
    commaSep (map summary od)

-- | A summary of a Reserved recard (many different).
instance Summarizable Reserved where
  summary (Reserved _ r) = "Unused " <> summary r
  summary (UsedReserved _ r is) =
    "Used " <> summary r <> " by " <>
    (T.pack $ show $ length is) <>
    " nodes [" <>
    commaSep (map summary is) <>
    "]"
  summary (MoveReserved _ r is) =
    "Move " <>
    (summary r) <>
    " for " <>
    (T.pack $ show $ length is) <>
    " nodes [" <>
    commaSep (map summary is) <>
    "]"
  summary (SplitReserved _ r is nis) =
    "Split " <>
    (summary r) <>
    " used by " <>
    (T.pack $ show $ length is) <>
    " nodes [" <>
    commaSep (map summary is) <>
    "] by adding " <>
    (T.pack $ show $ length nis) <>
    " more nodes [" <>
    commaSep (map summary nis) <>
    "]"
  summary (CombineReserved _ rs) =
    "Combine [" <>
    commaSep (map summary rs) <>
    "]"
  summary (ResizeReserved _ r is) =
    "Resize [" <>
    (summary r) <>
    " for " <>
    (T.pack $ show $ length is) <>
    " nodes [" <>
    commaSep (map summary is) <>
    "]"

-- | A summary of a OnDemand recard.
instance Summarizable OnDemand where
  summary = summary . view odInstance

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

{- Misc -}

{-
NOTE: This is probably a better way to model OnDemand & Reserved so we
can use the types & pattern match against them super expresively
without having to resort to these tedious filter functions.  More on
that in V2.  We need working over "perfectly modeled" at the moment.
-}

isReserved :: Reserved -> Bool
isReserved (Reserved{..}) = True
isReserved _ = False

isUsedReserved :: Reserved -> Bool
isUsedReserved (UsedReserved{..}) = True
isUsedReserved _ = False

isMoveReserved :: Reserved -> Bool
isMoveReserved (MoveReserved{..}) = True
isMoveReserved _ = False

isSplitReserved :: Reserved -> Bool
isSplitReserved (SplitReserved{..}) = True
isSplitReserved _ = False

isCombineReserved :: Reserved -> Bool
isCombineReserved (CombineReserved{..}) = True
isCombineReserved _ = False

isResizeReserved :: Reserved -> Bool
isResizeReserved (ResizeReserved{..}) = True
isResizeReserved _ = False

isModifiedReserved :: Reserved -> Bool
isModifiedReserved r =
  (isSplitReserved r || isCombineReserved r || isMoveReserved r ||
                                               isResizeReserved r)
