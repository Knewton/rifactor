{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-auto-orphans #-}

-- Module      : Rifactor.Types
-- Copyright   : (c) 2015 Knewton, Inc <se@knewton.com>
--               (c) 2015 Tim Dysinger <tim@dysinger.net> (contributor)
-- License     : Apache 2.0 http://opensource.org/licenses/Apache-2.0
-- Maintainer  : Tim Dysinger <tim@dysinger.net>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Rifactor.Types where

import Control.Lens (makeLenses)
import Data.Aeson.TH (deriveJSON)
import Network.AWS.Types
import Rifactor.Types.Internal

data Options =
  Plan {_configFile :: FilePath
       ,_logLevel :: String}

data Config =
  Config {_accounts :: [Account]
         ,_regions :: [Region]}

data Account =
  Account {_name :: String
          ,_accessKey :: String
          ,_secretKey :: String}

{- Lenses -}

$(makeLenses ''Account)
$(makeLenses ''Config)
$(makeLenses ''Options)

{- JSON -}

$(deriveJSON deriveOptions ''Account)
$(deriveJSON deriveOptions ''Config)
$(deriveJSON deriveOptions ''Options)
$(deriveJSON deriveOptions ''Region)
