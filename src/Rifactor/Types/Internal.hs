-- Module      : Rifactor.Types.Internal
-- Copyright   : (c) 2015 Knewton, Inc <se@knewton.com>
--               (c) 2015 Tim Dysinger <tim@dysinger.net> (contributor)
-- License     : Apache 2.0 http://opensource.org/licenses/Apache-2.0
-- Maintainer  : Tim Dysinger <tim@dysinger.net>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Rifactor.Types.Internal where

import Data.Aeson.TH (defaultOptions, fieldLabelModifier)
import Data.Aeson.Types (Options(..))
import Data.Char (toLower, isUpper)

deriveOptions :: Options
deriveOptions =
  defaultOptions {fieldLabelModifier =
                    camelToSnake .
                    drop 1}

camelToSnake :: String -> String
camelToSnake [] = []
camelToSnake (x:xs) = toLower x : snake xs
  where snake [] = []
        snake (y:ys)
          | isUpper y = '_' : toLower y : snake ys
          | otherwise = y : snake ys
