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

-- | Config is a combination of accounts & regions we want to cover.
-- It looks like so:
--
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

data Reserved =
  Reserved {_reEnv :: Env
           ,_reReserved :: ReservedInstances
           ,_reInstances :: [Instance]
           ,_reNewInstances :: [Instance]}
  deriving (Eq)

data Combine =
  Combine {_coReserved :: [Reserved]}
  deriving (Eq)

data Model =
  Model {_onDemand :: [OnDemand]
        ,_reserved :: [Reserved]
        ,_combined :: [Combine]}
  deriving (Eq)

-- | A Transition represents a state transition between two models.
-- It is the type signature of several functions that transform the
-- Model.
type Transition = Model -> Model

data InstanceGroup
  = C1
  | C2
  | C3
  | C4
  | CC1
  | CC2
  | CG1
  | CR1
  | G2
  | HI1
  | HS1
  | HS2
  | I2
  | M1
  | M2
  | M3
  | R3
  | T1
  | T2
  deriving (Show,Eq,Enum)

data InstanceSize
  = Micro
  | Small
  | Medium
  | Large
  | XLarge
  | XLarge2X
  | XLarge4X
  | XLarge8X
  deriving (Show,Eq,Enum)

{-
Lens: These declarations are here to generate code for us.  They
generate "lens" which gives us really powerful ways of view & updating
our records.
-}

$(makeLenses ''Account)
$(makeLenses ''Combine)
$(makeLenses ''Config)
$(makeLenses ''Model)
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
  summary (Reserved _ r is nis) =
    "Reserved " <>
    (summary r) <>
    (if length is > 0
        then " used by " <>
             (T.pack $ show $ length is) <>
             " [" <>
             sep (map summary is) <>
             "]"
        else "") <>
    (if length nis > 0
        then " adjusting for " <>
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
