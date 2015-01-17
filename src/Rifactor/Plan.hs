{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

-- Module      : Rifactor.Plan
-- Copyright   : (c) 2015 Knewton, Inc <se@knewton.com>
--               (c) 2015 Tim Dysinger <tim@dysinger.net> (contributor)
-- License     : Apache 2.0 http://opensource.org/licenses/Apache-2.0
-- Maintainer  : Tim Dysinger <tim@dysinger.net>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Rifactor.Plan where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Error hiding (Error)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS hiding (accessKey, secretKey)
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Resource
import           Data.Aeson as A
import qualified Data.Aeson.Parser as A
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as B
import           Data.Conduit
import qualified Data.Conduit.Attoparsec as C
import qualified Data.Conduit.Binary as C
import qualified Data.Conduit.List as C
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time.Clock.POSIX
import           Data.UUID
import           Data.UUID.V4
import           Network.AWS.Data
import           Network.AWS.EC2
import           Network.AWS.Types
import           Rifactor.Types
import           System.IO

default (Builder)

plan :: Options -> IO ()
plan opts =
  do config <- runResourceT $
               C.sourceFile (opts ^. configFile) $$
               C.sinkParser A.json
     case (A.fromJSON config) of
       (A.Error err) ->
         putStrLn ("Config File Error: " ++ err)
       (A.Success cfg) ->
         do lgr <- newLogger Info stdout
            uuid <- nextRandom
            fetchData opts cfg lgr uuid

credentials account =
  (FromKeys (AccessKey (B.pack (account ^. accessKey)))
            (SecretKey (B.pack (account ^. secretKey))))

environments lgr reg account =
  getEnv reg (credentials account) <&>
  envLogger .~
  lgr

fetchData opts cfg lgr uuid =
  forM_ (cfg ^. regions)
        (\reg ->
           do envs <- mapM (environments lgr reg) (cfg ^. accounts)
              reservations <- allRunningReservations envs
              let instances =
                    (foldl (\a x -> a ++ x ^. rInstances) [] reservations)
              rinstances <- allReservedInstances envs
              forM_ rinstances print
              mods <- allReservedInstancesModifications envs
              forM_ mods print)

allRunningReservations envs =
  foldM (\acc env ->
           do e <- envRunningReservations env
              case e of
                (Left err) ->
                  do print err
                     return acc
                (Right xs) -> return (acc ++ xs))
        []
        envs

envRunningReservations =
  flip runAWST
       (view dirReservations <$>
        send (describeInstances & di1Filters .~
              [filter' "instance-state-name" &
               fValues .~
               [toText ISNRunning]]))

allReservedInstances envs =
  foldM (\acc env ->
           do e <- envReservedInstances env
              case e of
                (Left err) ->
                  do print err
                     return acc
                (Right xs) -> return (acc ++ xs))
        []
        envs

envReservedInstances =
  flip runAWST
       (view drirReservedInstances <$>
        send (describeReservedInstances))

allReservedInstancesModifications envs =
  foldM (\acc env ->
           do e <- envReservedInstancesModifications env
              case e of
                (Left err) ->
                  do print err
                     return acc
                (Right xs) -> return (acc ++ xs))
        []
        envs

envReservedInstancesModifications =
  flip runAWST
       (view drimrReservedInstancesModifications <$>
        send (describeReservedInstancesModifications))

{-

Plan:
-- map over accounts in the same region
   -- map over all non-retired reserved instances in the region
   -- are there any unused?
   -- what instances could they be applied to?
      -- find instances to match a RI (in the same region)
         -- change the AZ to match (if needed)
         -- change the network platform to match (if needed)
         -- change the instance type to match (if needed)

-}
