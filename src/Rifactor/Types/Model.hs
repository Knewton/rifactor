{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- Module      : Rifactor.Types.Model
-- Copyright   : (c) 2015 Knewton, Inc <se@knewton.com>
--               (c) 2015 Tim Dysinger <tim@dysinger.net> (contributor)
-- License     : Apache 2.0 http://opensource.org/licenses/Apache-2.0
-- Maintainer  : Tim Dysinger <tim@dysinger.net>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Rifactor.Types.Model where

import BasePrelude
import Control.Lens
import Data.Aeson.TH (deriveToJSON)
import Data.Text (Text)
import Rifactor.Types.Internal (deriveOptions)

data Env a =
  Env {_env :: a
      ,_envName :: Text}
  deriving (Eq, Show)

data Instance a b =
  Instance {_insEnv :: Env a
           ,_insInst :: b}
  deriving (Eq, Show)

data Reserved a b =
  Reserved {_resEnv :: Env a
           ,_resResv :: b}
  deriving (Eq, Show)

data Used a b =
  Used {_used :: a
       ,_usedBy :: [b]}
  deriving (Eq, Show)

data Split a b =
  Split {_split :: a
        ,_splitBy :: [b]}
  deriving (Eq, Show)

data Combine a =
  Combine {_combine :: [a]}
  deriving (Eq, Show)

type UsedReserved e r i = Used (Reserved e r) (Instance e i)
type SplitReserved e r i = Split (Reserved e r) (Instance e i)
type SplitUsedReserved e r i = Split (UsedReserved e r i) (Instance e i)
type CombineReserved e r = Combine (Reserved e r)

data Model e r i =
  Model {_insts :: [Instance e i]
        ,_resvs :: [Reserved e r]
        ,_usedResvs :: [UsedReserved e r i]
        ,_splitResvs :: [SplitReserved e r i]
        ,_splitUsedResvs :: [SplitUsedReserved e r i]
        ,_combineResvs :: [CombineReserved e r]}

type Transition a = a -> a

class Matchable a b where
  matchable :: a -> b -> Bool

class Combineable a b where
  combineable :: a -> b -> Bool

class Splittable a b where
  splittable :: a -> b -> Bool

class Mergeable a b c where
  merge :: a -> b -> c

class Capacity a where
  capacityUsed :: a -> Maybe Float
  capacityTotal :: a -> Maybe Float

{- LENS -}

$(makeLenses ''Combine)
$(makeLenses ''Env)
$(makeLenses ''Instance)
$(makeLenses ''Model)
$(makeLenses ''Reserved)
$(makeLenses ''Split)
$(makeLenses ''Used)

{- JSON -}

$(deriveToJSON deriveOptions ''Combine)
$(deriveToJSON deriveOptions ''Env)
$(deriveToJSON deriveOptions ''Instance)
$(deriveToJSON deriveOptions ''Model)
$(deriveToJSON deriveOptions ''Reserved)
$(deriveToJSON deriveOptions ''Split)
$(deriveToJSON deriveOptions ''Used)

{- Instances -}

instance Foldable (Instance a) where
  foldr f y x@Instance{..} = f (x ^. insInst) y

instance Foldable (Reserved a) where
  foldr f y x@Reserved{..} = f (x ^. resResv) y

instance Foldable (Used a) where
  -- TODO how to fold into _used also?
  foldr f y x@Used{..} = foldr f y (x ^. usedBy)

instance Foldable (Split a) where
  foldr f y x@Split{..} = foldr f y (x ^. splitBy)

instance Foldable Combine where
  foldr f y x@Combine{..} = foldr f y (x ^. combine)
