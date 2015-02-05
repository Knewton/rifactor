{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
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

module Rifactor.Plan where

import           BasePrelude hiding (getEnv)
import           Control.Lens
import           Control.Monad.IO.Class ()
import           Control.Monad.Trans.AWS hiding (accessKey, secretKey)
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Aeson as A
import           Data.Conduit (($$))
import qualified Data.Conduit.Attoparsec as C (sinkParser)
import qualified Data.Conduit.Binary as C (sourceFile)
import           Network.AWS.EC2
import           Rifactor.AWS
import           Rifactor.Types
import           System.IO (stdout)

-- | Read in the config file (JSON). If we failed to read the file,
-- exit. If we read it OK, then continue on to querying Amazon for
-- pending reserverd instance modifications.
--
-- If we have any of ReservedInstanceModifications in progress then we
-- have to stop right here. It's too hard to reason about Amazon
-- accepting or denying any of our change requests. We can't really
-- build a plan on top of a mystery outcome. We'll just wait this
-- round if that's the case.
--
-- If we don't have any pending modifications, then we get on with the
-- show. We query Amazon for data on EC2 Instances &
-- ReservedInstances in every Region in the config file & for every
-- set of Amazon IAM credentials in the account.
--
-- We then build a model in memory of what's going on over at Amazon
-- WRT which instances actually count against our ReservedInstances &
-- which ones don't.
plan :: Options -> IO ()
plan opts =
  do config <-
       runResourceT $
       C.sourceFile (opts ^. file) $$
       C.sinkParser A.json
     case (A.fromJSON config :: A.Result Config) of
       (A.Error err) -> putStrLn err >> exitFailure
       (A.Success cfg) ->
         do lgr <-
              newLogger (if opts ^. verbose
                            then Trace
                            else Info)
                        stdout
            env' <- noKeysEnv
            es <- initEnvs cfg lgr
            pending <-
              runAWST env' (checkPendingModifications es)
            case pending of
              (Left err) -> print err >> exitFailure
              _ ->
                do results <-
                     runAWST env' (fetchFromAmazon es)
                   case results of
                     (Left err) -> print err >> exitFailure
                     (Right m) ->
                       do let result = transition m
                          traverse_ (print . summary)
                                    (filter (not . null . view reNewInstances)
                                            (result ^. reserved))
                          traverse_ (print . summary)
                                    (result ^. combined)

-- TODO print a nice summary of instances & reservations groupBy
-- (region, az, instance type, network-type)

-- | Take an initial Model and then transition it through steps &
-- return the new Model.
transition :: Transition
transition = packReserved . combineReserved . matchReserved

-- | Match Reserved with OnDemand nodes instance type, network type &
-- availability zone.
matchReserved :: Transition
matchReserved = mergeInstances matchFn mergeFn
  where matchFn (Reserved _ r _ _) (OnDemand _ i) =
          (r ^. ri1AvailabilityZone == i ^. i1Placement ^. pAvailabilityZone) &&
          (r ^. ri1InstanceType == i ^? i1InstanceType)
        mergeFn (Reserved e r is nis) (OnDemand _ i) =
          (Reserved e r (i : is) nis)

packReserved :: Transition
packReserved = mergeInstances matchFn mergeFn
  where matchFn _ _ = True
        mergeFn (Reserved e r is nis) (OnDemand _ i) =
          (Reserved e r is (i : nis))

-- | Of the Reserved Instances that aren't currently being used,
-- combine RIs where conditions permit (see Amazon specs).
combineReserved :: Transition
combineReserved (Model os rs cs) =
  go (groupBy matches rs) [] cs
  where go [] rs' cs' = Model os rs' cs'
        go (g:gs) rs' cs' =
          if length g > 1
             then go gs rs' (cons (Combine g) cs')
             else go gs (g ++ rs') cs'
        matches (Reserved er0 r0 _ _) (Reserved er1 r1 _ _) =
          (r0 ^. ri1End == r1 ^. ri1End) &&
          (r0 ^. ri1OfferingType == r1 ^. ri1OfferingType) &&
          (r0 ^. ri1InstanceType == r1 ^. ri1InstanceType) &&
          (er0 ^. envRegion == er1 ^. envRegion)

-- | This function is an abstraction. We repeatedly need to take a
-- Reserved that we know little about & associate OnDemand instances
-- with it based on criteria.
--
-- Using the supplied first-argument function, find the Reserved that
-- match. From that list look for any OnDemand that match the
-- second-argument function. If there is a match we pass Reserved data
-- to the 3rd function which constructs a new Reserved record (which
-- we exchange for the prevous Reserved record in the Model).
--
-- This is repeated for every Reserved in the Model. At the end of the
-- recursion you are left with a new version of the Model.
mergeInstances :: (Reserved -> OnDemand -> Bool)
               -> (Reserved -> OnDemand -> Reserved)
               -> Transition
mergeInstances matchFn mergeFn (Model os rs cs) =
  go os rs []
  where go [] ys zs = Model [] (ys ++ zs) cs
        go xs [] zs = Model xs zs cs
        go xs (y:ys) zs =
          case partition (matchFn y) xs of
            ([],_) -> go xs ys (y : zs)
            (hits,misses) ->
              let (result,rejects) =
                    foldl (\(r,rejected) o ->
                             if (isInstanceTypeMatch r o)
                                then (mergeFn r o,rejected)
                                else (r,o : rejected))
                          (y,[])
                          hits
              in go (misses ++ rejects)
                    ys
                    (result : zs)

-- TODO We need to auto-resolve reservation total capacity with used vs new instances.
-- TODO we need to replay the model on AWS

{-

Executing Changes

When we are just changing AZ for the same instance size, go for it.

When we are just changing network for the same instance size, go for
it.

When we have plenty of capacity then just take what we need for new
instances & leave the spare capacity like it is now.

When we can't 'balance the books' do nothing

-}

doUpdateReserved r | isNothing (r ^. reReserved ^. ri1ReservedInstancesId) = undefined
doUpdateReserved r =
  do let config =
           [(reservedInstancesConfiguration &
             (ricAvailabilityZone ?~
              T.pack "us-east-1c") &
             (ricInstanceType ?~ M2_4XLarge) &
             (ricPlatform ?~ "EC2-Classic") &
             (ricInstanceCount ?~ 2))
           ,(reservedInstancesConfiguration &
             (ricAvailabilityZone ?~
              T.pack "us-east-1b") &
             (ricInstanceType ?~ M2_4XLarge) &
             (ricPlatform ?~ "EC2-Classic") &
             (ricInstanceCount ?~ 2))]
     runAWST (r ^. reEnv)
             (send (modifyReservedInstances &
                    (mriReservedInstancesIds .~
                     [r ^. reReserved ^. ri1ReservedInstancesId ^?! _Just]) &
                    (mriClientToken ?~ "") &
                    (mriTargetConfigurations .~ config))) >>=
       hoistEither

doCombineReserved = undefined
