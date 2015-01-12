module Main where

import Data.Either (Either(..))
import Data.Foreign (parseJSON, unsafeFromForeign)
import Debug.Trace (print)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)

import Rifactor.Types

main = do
  txt <- readTextFile UTF8 "etc/config.json"
  case parseJSON txt of
    Left err -> print err
    Right f -> do
      let config = unsafeFromForeign f
      print "config loaded"
