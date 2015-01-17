{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- Module      : Rifactor.Plan
-- Copyright   : (c) 2015 Knewton, Inc <se@knewton.com>
--               (c) 2015 Tim Dysinger <tim@dysinger.net> (contributor)
-- License     : Apache 2.0 http://opensource.org/licenses/Apache-2.0
-- Maintainer  : Tim Dysinger <tim@dysinger.net>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

import Control.Monad.Trans.AWS (Error)
import Options.Applicative
import Rifactor.Plan
import Rifactor.Types

planParserInfo :: ParserInfo Options
planParserInfo =
  info (helper <*>
        (Plan <$>
         (option auto
                 (long "config-file" <>
                  short 'c' <>
                  metavar "FILE" <>
                  help "Read config from FILE") <|>
          pure "config.json") <*>
         (option auto
                 (long "log-level" <>
                  short 'l' <>
                  metavar "LEVEL" <>
                  help "Set the logging LEVEL") <|>
          pure "info")))
       (fullDesc <>
        header "RIFactor plan" <>
        progDesc "Discover state, plan savings & print the plan")

mainParserInfo :: ParserInfo Options
mainParserInfo =
  info (helper <*>
        subparser (command "plan" planParserInfo))
       (fullDesc <>
        header "RIFactor" <>
        progDesc "Optimize AWS Reserved Instances")

exec opts@Plan{..} = plan opts

main = exec =<< execParser mainParserInfo
