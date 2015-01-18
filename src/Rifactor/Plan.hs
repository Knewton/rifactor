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
           do envs <- mapM (environments lgr reg)
                           (cfg ^. accounts)
              putStrLn (T.unpack (toText reg) ++
                        " for all accounts")
              reservations <- allRunningReservations envs
              let instances =
                    (foldl (\a x -> a ++ x ^. rInstances) [] reservations)
              rinst <- allReservedInstances envs
              putStrLn ("\t" ++
                        show (length rinst) ++
                        " reserved instances")
              mods <- allReservedInstancesModifications envs
              putStrLn ("\t" ++
                        show (length mods) ++
                        " reserved instances modifications"))

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
        send (describeReservedInstances & driFilters .~
              [filter' "state" &
               fValues .~
               [toText RISActive]]))

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

Solver: (repeat every 5 minutes)
-- for all accounts in the same region
   -- for all non-retired non-100% utilized reserved instances (RI)
      -- create a plan for each
         -- if there isn't a modification in progress for this RI
            -- position the RI for the biggest pool of nodes we can find
               -- add a pool of instances that match the RI exactly already
               -- modify the RI to be ready for bigger pools of nodes
                  -- search (sorted) & on availability-zone, network & instance type
               -- release all instances from any sub-optimal RI
                  EG, 6 node RI is in 1C, only 2 'VPC' nodes in 1C. 9 dangling 'CLASSIC' nodes in 1A.
               -- reduce instances from algorithm state (ones listed in RI modifications)

-}
