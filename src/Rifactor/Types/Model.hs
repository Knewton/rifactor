{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NoImplicitPrelude #-}
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
  deriving (Eq,Foldable,Functor,Show,Traversable)

$(makeLenses ''Env)

data Resource e r i
  = Reserved {_resEnv :: Env e
             ,_resource :: r}
  | Instance {_resEnv :: Env e
             ,_instance :: i}
  deriving (Eq,Foldable,Functor,Show,Traversable)

$(makeLenses ''Resource)

data Model a
  = Empty
  | Item {_item :: a}
  | Used {_used :: Model a
         ,_by :: [Model a]}
  | Merge {_merged :: [Model a]}
  | Split {_split :: Model a
          ,_by :: [Model a]}
  | Full {_full :: Model a}
  | Model {_model :: [Model a]}
  deriving (Eq,Foldable,Functor,Show,Traversable)

$(makeLenses ''Model)

type Transition a = a -> a

$(deriveToJSON deriveOptions ''Env)
$(deriveToJSON deriveOptions ''Resource)
$(deriveToJSON deriveOptions ''Model)
