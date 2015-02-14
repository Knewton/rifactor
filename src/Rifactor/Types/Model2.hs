{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

-- Module      : Rifactor.Types.Model2
-- Copyright   : (c) 2015 Knewton, Inc <se@knewton.com>
--               (c) 2015 Tim Dysinger <tim@dysinger.net> (contributor)
-- License     : Apache 2.0 http://opensource.org/licenses/Apache-2.0
-- Maintainer  : Tim Dysinger <tim@dysinger.net>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Rifactor.Types.Model2 where

import BasePrelude
import Control.Lens
import Data.Text (Text)

data Env a =
  Env {_env :: a
      ,_envName :: Text}
  deriving (Eq,Ord,Data,Functor,Foldable,Generic,Read,Show,Traversable,Typeable)

$(makeLenses ''Env)

data Resource e r i
  = Reserved {_resEnv :: Env e
             ,_reserved :: r}
  | Instance {_resEnv :: Env e
             ,_instance :: i}
  deriving (Eq,Ord,Data,Functor,Foldable,Generic,Read,Show,Traversable,Typeable)

$(makeLenses ''Resource)

data Manifest a
  = None
  | Some {_some :: [Manifest a]}
  | Used {_used :: Manifest a
         ,_usedBy :: [Manifest a]}
  | Full {_full :: Manifest a}
  | Split {_split :: Manifest a
          ,_splitBy :: [Manifest a]}
  | Combined {_combined :: [Manifest a]}
  deriving (Eq,Ord,Data,Functor,Foldable,Generic,Read,Show,Traversable,Typeable)

$(makeLenses ''Manifest)
