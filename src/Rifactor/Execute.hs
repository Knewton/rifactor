{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Rifactor.Execute
-- Copyright   : (c) 2015 Knewton, Inc <se@knewton.com>
--               (c) 2015 Tim Dysinger <tim@dysinger.net> (contributor)
-- License     : Apache 2.0 http://opensource.org/licenses/Apache-2.0
-- Maintainer  : Tim Dysinger <tim@dysinger.net>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Rifactor.Execute where

import           BasePrelude
import           Control.Lens
import qualified Control.Monad.Trans.AWS as AWS
import           Control.Monad.Trans.AWS hiding (Env)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Conduit (($$))
import           Data.Conduit.Attoparsec (sinkParser)
import           Data.Conduit.Binary (sourceFile)
import           Data.Text (Text)
import qualified Network.AWS.Data as AWS
import qualified Network.AWS.EC2 as EC2
import           Network.AWS.EC2 hiding (Error,Instance,Region)
import           Rifactor.AWS
import           Rifactor.Types
import           System.IO (stdout)

default (Text)

plan :: Options -> IO ()
plan opts =
  do config <-
       runResourceT $
       sourceFile (opts ^. confFile) $$
       sinkParser json
     case (fromJSON config :: Result Config) of
       (Error err) -> putStrLn err >> exitFailure
       (Success cfg) ->
         do lgr <-
              newLogger (if opts ^. verbose
                            then Trace
                            else Info)
                        stdout
            noEnv <- noKeysEnv
            envs <- initEnvs cfg lgr
            pending <-
              runAWST (noEnv ^. env)
                      (checkPendingModifications envs)
            case pending of
              (Left err) -> print err >> exitFailure
              _ ->
                do fetch <-
                     runAWST (noEnv ^. env)
                             (fetchFromAmazon envs)
                   case fetch of
                     (Left err) -> print err >> exitFailure
                     (Right m) ->
                       do let m' = transition m
                          when (opts ^. verbose)
                               (LB.putStrLn (encode m'))

transition :: AwsModelTransition
transition = splitReserved . combineReserved . matchReserved

matchReserved :: AwsModelTransition
matchReserved m = undefined
  -- solve m (m & resvs .~ [] & usedResvs .~ [])
  -- where solve old new
  --         | not (null (old ^. usedResvs)) =
  --           -- used + instance = used
  --           let (u:us) = old ^. usedResvs
  --           in case partition (matchable u)
  --                             (new ^. insts) of
  --                ([],_) ->
  --                  solve (old & usedResvs .~ us)
  --                        (new & usedResvs %~ (|> u))
  --                (h:t,nomatch) ->
  --                  case merge u h of
  --                    Just u' ->
  --                      solve (old & usedResvs .~ u' : us)
  --                            (new & insts .~ t ++ nomatch)
  --                    Nothing ->
  --                      solve (old & usedResvs .~ us)
  --                            (new & usedResvs %~ (|> u))
  --       solve old new
  --         | not (null (old ^. resvs)) =
  --           -- reserved + instance = used
  --           let (r:rs) = old ^. resvs
  --           in case partition (matchable r)
  --                             (new ^. insts) of
  --                ([],_) ->
  --                  solve (old & resvs .~ rs)
  --                        (new & resvs %~ (|> r))
  --                (h:t,nomatch) ->
  --                  case merge r h of
  --                    Just u ->
  --                      solve (old & resvs .~ rs & usedResvs %~ (|> u))
  --                            (new & insts .~ t ++ nomatch)
  --                    Nothing ->
  --                      solve (old & resvs .~ rs)
  --                            (new & resvs %~ (|> r))
  --       solve _ new = new

combineReserved :: AwsModelTransition
combineReserved m = undefined
  -- solve m (m & resvs .~ [] & combineResvs .~ [])
  -- where solve old new
  --         | not (null (old ^. combineResvs)) =
  --           -- combine + reserved = combine
  --           let (c:cs) = old ^. combineResvs
  --           in case partition (combineable c)
  --                             (old ^. resvs) of
  --                ([],_) ->
  --                  solve (old & combineResvs .~ cs)
  --                        (new & combineResvs %~ (|> c))
  --                (h:t,nomatch) ->
  --                  case merge c h of
  --                    Just c' ->
  --                      solve (old &
  --                             (resvs .~ t ++ nomatch) &
  --                             (combineResvs .~ c' : cs))
  --                            new
  --                    Nothing ->
  --                      solve (old & combineResvs .~ cs)
  --                            (new & combineResvs %~ (|> c))
  --       solve old new
  --         | not (null (old ^. resvs)) =
  --           -- reserved + reserved = combine
  --           let (r:rs) = old ^. resvs
  --           in case partition (combineable r) rs of
  --                ([],_) ->
  --                  solve (old & resvs .~ rs)
  --                        (new & resvs %~ (|> r))
  --                (h:t,nomatch) ->
  --                  case merge r h of
  --                    Just c ->
  --                      solve (old &
  --                             (resvs .~ t ++ nomatch) &
  --                             (combineResvs %~ (|> c)))
  --                            new
  --                    Nothing ->
  --                      solve (old & resvs .~ rs)
  --                            (new & resvs %~ (|> r))
  --       solve _ new = new

splitReserved :: AwsModelTransition
splitReserved m = undefined
  -- solve m (m & resvs .~ [] & splitResvs .~ [])
  -- where solve old new
  --         | not (null (old ^. splitResvs)) =
  --           -- split + instance = split
  --           let (s:ss) = old ^. splitResvs
  --           in case partition (splittable s)
  --                             (new ^. insts) of
  --                ([],_) ->
  --                  solve (old & splitResvs .~ ss)
  --                        (new & splitResvs %~ (|> s))
  --                (h:t,nomatch) ->
  --                  case merge s h of
  --                    Just s' ->
  --                      solve (old & splitResvs .~ s' : ss)
  --                            (new & insts .~ t ++ nomatch)
  --                    Nothing ->
  --                      solve (old & splitResvs .~ ss)
  --                            (new & splitResvs %~ (|> s))
  --       solve old new
  --         | not (null (old ^. resvs)) =
  --           -- reserved + instance = split
  --           let (r:rs) = old ^. resvs
  --           in case partition (splittable r)
  --                             (new ^. insts) of
  --                ([],_) ->
  --                  solve (old & resvs .~ rs)
  --                        (new & resvs %~ (|> r))
  --                (h:t,nomatch) ->
  --                  case merge r h of
  --                    Just s ->
  --                      solve (old & resvs .~ rs & splitResvs %~ (|> s))
  --                            (new & insts .~ t ++ nomatch)
  --                    Nothing ->
  --                      solve (old & resvs .~ rs)
  --                            (new & resvs %~ (|> r))
  --       solve _ new = new

splitUsedReserved :: AwsModelTransition
splitUsedReserved m = undefined
  -- solve m (m & usedResvs .~ [] & splitUsedResvs .~ [])
  -- where solve old new
  --         | not (null (old ^. splitUsedResvs)) =
  --           -- split-used + instance = split-used
  --           let (s:ss) = old ^. splitUsedResvs
  --           in case partition (splittable s)
  --                             (new ^. insts) of
  --                ([],_) ->
  --                  solve (old & splitUsedResvs .~ ss)
  --                        (new & splitUsedResvs %~ (|> s))
  --                (h:t,nomatch) ->
  --                  case merge s h of
  --                    Just s' ->
  --                      solve (old & splitUsedResvs .~ s' : ss)
  --                            (new & insts .~ t ++ nomatch)
  --                    Nothing ->
  --                      solve (old & splitUsedResvs .~ ss)
  --                            (new & splitUsedResvs %~ (|> s))
  --       solve old new
  --         | not (null (old ^. usedResvs)) =
  --           -- used + instance = split-used
  --           let (u:us) = old ^. usedResvs
  --           in case partition (splittable u)
  --                             (new ^. insts) of
  --                ([],_) ->
  --                  solve (old & usedResvs .~ us)
  --                        (new & usedResvs %~ (|> u))
  --                (h:t,nomatch) ->
  --                  case merge u h of
  --                    Just u ->
  --                      solve (old & usedResvs .~ us & splitUsedResvs %~ (|> u))
  --                            (new & insts .~ t ++ nomatch)
  --                    Nothing ->
  --                      solve (old & usedResvs .~ us)
  --                            (new & usedResvs %~ (|> u))
  --       solve _ new = new

-- TODO print a summary of changes
-- TODO equalize combined
-- TODO execute reserved instance modifications
