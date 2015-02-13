{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

-- Module      : Rifactor.Types.Options
-- Copyright   : (c) 2015 Knewton, Inc <se@knewton.com>
--               (c) 2015 Tim Dysinger <tim@dysinger.net> (contributor)
-- License     : Apache 2.0 http://opensource.org/licenses/Apache-2.0
-- Maintainer  : Tim Dysinger <tim@dysinger.net>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Rifactor.Types.Options where

import BasePrelude
import Control.Lens

data Options =
  Options {_confFile :: FilePath
          ,_dryRun :: Bool
          ,_verbose :: Bool}

{- LENS -}

$(makeLenses ''Options)
