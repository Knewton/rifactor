{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  Env {_eEnv :: a
      ,_eName :: Text}
  deriving (Eq,Foldable,Functor,Show,Traversable)

data Resource e r i
  = Reserved {_rEnv :: Env e
             ,_rReserved :: r}
  | Instance {_rEnv :: Env e
             ,_rInstance :: i}
  deriving (Eq,Foldable,Functor,Show,Traversable)

data Plan a
  = Noop
  | Item {_item :: a}
  | Used {_used :: Plan a
         ,_usedBy :: [Plan a]}
  | Merge {_merged :: [Plan a]}
  | Split {_split :: Plan a
          ,_splitBy :: [Plan a]}
  | Plans {_plans :: [Plan a]}
  deriving (Eq,Foldable,Functor,Show,Traversable)

type Transition a = a -> a

$(makeClassy ''Env)
$(makeClassy ''Resource)
$(makeClassy ''Plan)

$(makeClassyPrisms ''Plan)

$(deriveToJSON deriveOptions ''Env)
$(deriveToJSON deriveOptions ''Resource)
$(deriveToJSON deriveOptions ''Plan)
