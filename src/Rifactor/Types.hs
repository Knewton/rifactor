{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

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

-- | OnDemand represents Instancs that we haven't found a match (with
-- ReservedInstances) yet.  We can also track the Env that we got the
-- instance from in the record.
data OnDemand =
  OnDemand {_odEnv :: Env
           ,_odInstance :: Instance}
  deriving (Eq)

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

type Model = ([Reserved],[OnDemand])

type Transition = Model -> Model

ಠ_ಠ :: String -> a
ಠ_ಠ = error

{- Lenses -}

$(makeLenses ''Account)
$(makeLenses ''Config)
$(makeLenses ''OnDemand)
$(makeLenses ''Options)
$(makeLenses ''Reserved)

{- JSON -}

$(deriveJSON deriveOptions ''Account)
$(deriveJSON deriveOptions ''Config)
$(deriveJSON deriveOptions ''Options)
$(deriveJSON deriveOptions ''Region)

{- Classes/Instances -}

commaSep :: [T.Text] -> T.Text
commaSep = T.intercalate (T.pack ", ")

class Summarizable a where
  summary :: a -> T.Text

instance Summarizable Model where
  summary (rs,od) =
    "Model " <>
    commaSep (map summary rs) <>
    " " <>
    commaSep (map summary od)

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

instance Summarizable OnDemand where
  summary = summary . view odInstance

instance Summarizable ReservedInstances where
  summary x =
    fromMaybe T.empty (x ^. ri1ReservedInstancesId) <>
    "|" <>
    toText (fromMaybe 0 (x ^. ri1InstanceCount)) <>
    "|" <>
    maybe T.empty toText (x ^. ri1InstanceType) <>
    "|" <>
    fromMaybe T.empty (x ^. ri1AvailabilityZone)

instance Summarizable Instance where
  summary x =
    (x ^. i1InstanceId) <>
    "|" <>
    toText (x ^. i1InstanceType) <>
    "|" <>
    maybe T.empty toText (x ^. i1Placement ^. pAvailabilityZone)

{- Misc -}

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
