{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

-- Module      : Rifactor.Types.Config
-- Copyright   : (c) 2015 Knewton, Inc <se@knewton.com>
--               (c) 2015 Tim Dysinger <tim@dysinger.net> (contributor)
-- License     : Apache 2.0 http://opensource.org/licenses/Apache-2.0
-- Maintainer  : Tim Dysinger <tim@dysinger.net>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Rifactor.Types.Config where

import Control.Lens (makeLenses)
import Data.Aeson.TH (deriveFromJSON)
import Data.Text (Text)
import Network.AWS (Region)
import Rifactor.Types.Internal (deriveOptions)

default (Text)

data Account =
  Account {_name :: Text
          ,_accessKey :: Text
          ,_secretKey :: Text}

data Config =
  Config {_accounts :: [Account]
         ,_regions :: [Region]}

{- LENS -}

$(makeLenses ''Account)
$(makeLenses ''Config)

{- JSON -}

$(deriveFromJSON deriveOptions ''Region)
$(deriveFromJSON deriveOptions ''Account)
$(deriveFromJSON deriveOptions ''Config)
