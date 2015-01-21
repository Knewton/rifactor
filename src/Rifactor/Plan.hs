{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

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
import           Control.Monad.Trans.AWS hiding (accessKey, secretKey)
import           Control.Monad.Trans.Resource
import           Data.Aeson as A
import qualified Data.ByteString.Char8 as B
import           Data.Conduit
import qualified Data.Conduit.Attoparsec as C
import qualified Data.Conduit.Binary as C
import           Data.UUID.V4
import           Network.AWS.Data
import           Network.AWS.EC2
import           Rifactor.Types
import           System.IO

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

credentials :: Account -> Credentials
credentials account =
  FromKeys (AccessKey (B.pack (account ^. accessKey)))
           (SecretKey (B.pack (account ^. secretKey)))

fetchData :: forall t t1. t -> Config -> Logger -> t1 -> IO ()
fetchData _opts cfg lgr _uuid =
  do envs <- zipWithM (\acc reg ->
                         getEnv reg (credentials acc) <&>
                         envLogger .~
                         lgr)
                      (cfg ^. accounts)
                      (cfg ^. regions)
     reservations <- mapEnvM envRunningReservations envs
     let _instances =
           foldl (\a x -> a ++ x ^. rInstances) [] reservations
     _rinstances <- mapEnvM envReservedInstances envs
     -- TODO total reserved  instances by offering type, network type, availability-zone
     -- TODO total on-demand instances by offering type, network type, availability-zone
     return ()

mapEnvM :: forall e a b.
           Show e
        => (b -> IO (Either e [a])) -> [b] -> IO [a]
mapEnvM f envs =
  foldM (\acc env ->
           do e <- f env
              case e of
                (Left err) ->
                  do print err
                     return acc
                (Right xs) -> return (acc ++ xs))
        []
        envs

envRunningReservations :: Env -> IO (Either Error [Reservation])
envRunningReservations =
  flip runAWST
       (view dirReservations <$>
        send (describeInstances & di1Filters .~
              [filter' "instance-state-name" &
               fValues .~
               [toText ISNRunning]]))

envReservedInstances :: Env -> IO (Either Error [ReservedInstances])
envReservedInstances =
  flip runAWST
       (view drirReservedInstances <$>
        send (describeReservedInstances & driFilters .~
              [filter' "state" &
               fValues .~
               [toText RISActive]]))

envReservedInstancesModifications :: Env
                                  -> IO (Either Error [ReservedInstancesModification])
envReservedInstancesModifications =
  flip runAWST
       (view drimrReservedInstancesModifications <$>
        send describeReservedInstancesModifications)

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
