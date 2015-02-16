{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- Module      : Main
-- Copyright   : (c) 2015 Knewton, Inc <se@knewton.com>
--               (c) 2015 Tim Dysinger <tim@dysinger.net> (contributor)
-- License     : Apache 2.0 http://opensource.org/licenses/Apache-2.0
-- Maintainer  : Tim Dysinger <tim@dysinger.net>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

import BasePrelude
import Data.Time
import Distribution.PackageDescription.TH
import Git.Embed
import Language.Haskell.TH
import Options.Applicative
import Rifactor.Plan
import Rifactor.Types

version :: String
version = $(packageVariable (pkgVersion . package))

gitRev :: String
gitRev = $(embedGitShortRevision)

buildDate :: String
buildDate =
  $(stringE =<<
    runIO (show `fmap` Data.Time.getCurrentTime))

parserInfo :: ParserInfo Options
parserInfo =
  info (helper <*>
        (Options <$>
         (strOption (long "config-file" <>
                     short 'c' <>
                     metavar "FILE" <>
                     help "Read JSON config from FILE") <|>
          pure "config.json") <*>
         (switch (long "dry-run" <>
                  short 'd' <>
                  help "Print instead of running")) <*>
         (switch (long "verbose" <>
                  short 'v' <>
                  help "Show trace-level AWS data"))))
       (fullDesc <>
        header (("REFactor " ++ version) ++
                (" | Source: " ++ gitRev) ++
                (" | Built: " ++ buildDate)) <>
        progDesc "Optimize AWS Reserved Instances")

main :: IO ()
main = execParser parserInfo >>= exec
