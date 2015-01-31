{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
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

import           BasePrelude hiding (getEnv)
import           Control.Lens
import           Control.Monad.IO.Class ()
import           Control.Monad.Trans.AWS hiding (accessKey, secretKey)
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Aeson as A
import           Data.Conduit (($$))
import qualified Data.Conduit.Attoparsec as C (sinkParser)
import qualified Data.Conduit.Binary as C (sourceFile)
import qualified Data.Text as T
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
                     (Right xs) ->
                       do let (reserved,_) = transition xs
                          traverse_ (putStrLn . T.unpack . summary) reserved -- (filter isModifiedReserved reserved)
                                                                            -- TODO print a nice summary of instances & reservations groupBy (region, az, instance type, network-type)


-- | Take an initial Model and then transition it through steps &
-- return the new Model.
transition :: Transition
transition = combineReserved . packReserved . matchReserved

-- | Pack ReservedInstances as tightly as possible.
packReserved :: Transition
packReserved = splitReserved . moveReserved -- TODO replace with universal bin-packing algorithm

-- | Match unused ReservedInstances with OnDemand nodes that
-- match by instance type, network type & availability zone.
matchReserved :: Transition
matchReserved =
  mergeInstances isReserved isPerfectMatch convertToUsed
  where isPerfectMatch (Reserved er r) (OnDemand ei i) =
          (r ^. ri1AvailabilityZone == i ^. i1Placement ^. pAvailabilityZone) &&
          (r ^. ri1InstanceType == i ^? i1InstanceType) &&
          (er ^. envRegion == ei ^. envRegion)
        -- TODO Add network type (Classic vs VPN)
        isPerfectMatch _ _ = False
        convertToUsed r =
          UsedReserved (r ^. reEnv)
                       (r ^?! reReservedInstances)

-- | Move unused ReservedInstances around to accommidate nodes that
-- match by instance type.
moveReserved :: Transition
moveReserved =
  mergeInstances isReserved isWorkableMatch convertToMove
  where isWorkableMatch (Reserved er r) (OnDemand ei i) =
          (r ^. ri1InstanceType == i ^? i1InstanceType) &&
          (er ^. envRegion == ei ^. envRegion)
        isWorkableMatch _ _ = False
        convertToMove r =
          MoveReserved (r ^. reEnv)
                       (r ^?! reReservedInstances)

-- | Split used ReservedInstances up that have remaining capacity but
-- still have slots left for nodes with the same instance type but
-- with other availability zones or network types.
splitReserved :: Transition
splitReserved =
  mergeInstances isUsedReserved isWorkableMatch convertToSplit
  where isWorkableMatch (UsedReserved er r is) (OnDemand ei i) =
          (r ^. ri1InstanceType == i ^? i1InstanceType) &&
          (er ^. envRegion == ei ^. envRegion) &&
          maybe False (length is <) (r ^. ri1InstanceCount)
        isWorkableMatch _ _ = False
        convertToSplit r =
          SplitReserved (r ^. reEnv)
                        (r ^?! reReservedInstances)
                        (r ^?! reInstances)

-- | Of the Reserved Instances that aren't currently being modified,
-- combine RIs where conditions permit (see Amazon specs).
combineReserved :: Transition
combineReserved (reserved,onDemand) =
  let (modified,notModified) =
        partition isModifiedReserved reserved
      reducedNotModified =
        concatMap combine (groupBy isWorkableMatch notModified)
  in (modified ++ reducedNotModified,onDemand)
  where isWorkableMatch x y =
          ((x ^?! reReservedInstances ^. ri1End) ==
           (y ^?! reReservedInstances ^. ri1End)) &&
          ((x ^?! reReservedInstances ^. ri1OfferingType) ==
           (y ^?! reReservedInstances ^. ri1OfferingType)) &&
          -- TODO we have a little wiggle room in the instance type
          -- once we have our calculator ready
          ((x ^?! reReservedInstances ^. ri1InstanceType) ==
           (y ^?! reReservedInstances ^. ri1InstanceType)) &&
          ((x ^. reEnv ^. envRegion) ==
           (y ^. reEnv ^. envRegion))
        combine [] = []
        combine rs@[_] = rs
        combine rs@(r:_) =
          [CombineReserved (r ^. reEnv)
                           (map (^?! reReservedInstances) rs)]

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
mergeInstances :: (Reserved -> Bool)
               -> (Reserved -> OnDemand -> Bool)
               -> (Reserved -> [Instance] -> Reserved)
               -> Transition
mergeInstances isMatchingReserved isMatchingInstance convert (reserved,nodes) =
  let (unmatchedReserved,otherReserved) =
        partition isMatchingReserved reserved
  in go otherReserved (unmatchedReserved,nodes)
  where go rs ([],ys) = (rs,ys)
        go rs (xs,[]) = (rs ++ xs,[])
        go rs (x:xs,ys) =
          case partition (isMatchingInstance x) ys of
            ([],_) ->
              go (x : rs)
                 (xs,ys)
            (matched,unmatched) ->
              let (used,unused) =
                    splitAt (fromMaybe 0
                                       (x ^?! reReservedInstances ^.
                                        ri1InstanceCount))
                            matched
              in if null used
                    then go (x : rs)
                            (xs,matched ++ unmatched)
                    else go (convert x (map (view odInstance) used) :
                             rs)
                            (xs,unused ++ unmatched)
