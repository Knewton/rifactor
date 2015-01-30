{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

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

data Options =
  Options {_file :: FilePath
          ,_dry :: Bool
          ,_verbose :: Bool }

data Account =
  Account {_name :: String
          ,_accessKey :: String
          ,_secretKey :: String}

data Config =
  Config {_accounts :: [Account]
         ,_regions :: [Region]}

data OnDemand =
  OnDemand {_odInstance :: Instance}
  deriving (Eq)

instance Eq Env

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

class Summarizable a where
  summary :: a -> T.Text

instance Summarizable Reserved where
  summary (MoveReserved _ r is) =
    "move " <>
    (summary r) <>
    " for [" <>
    T.intercalate (T.pack ", ")
                  (map summary is) <>
    "]"
  summary _ = error "TODO Summerizeable Reserved pattern matching incomplete"

instance Summarizable OnDemand where
  summary x =
    "on-demand " <>
    summary (x ^. odInstance)

instance Summarizable ReservedInstances where
  summary x =
    "reserved-instances#" <>
    fromMaybe T.empty (x ^. ri1ReservedInstancesId) <>
    "|" <>
    maybe T.empty toText (x ^. ri1InstanceType) <>
    "|" <>
    fromMaybe T.empty (x ^. ri1AvailabilityZone)

instance Summarizable Instance where
  summary x =
    "instance#" <>
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
