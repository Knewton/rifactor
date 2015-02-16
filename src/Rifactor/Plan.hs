{-# LANGUAGE ExtendedDefaultRules #-}
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
exec :: Options -> IO ()
exec opts =
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
              runAWST (noEnv ^. eEnv)
                      (checkPendingModifications envs)
            case pending of
              (Left err) -> print err >> exitFailure
              _ ->
                do fetch <-
                     runAWST (noEnv ^. eEnv)
                             (fetchFromAmazon envs)
                   case fetch of
                     (Left err) -> print err >> exitFailure
                     (Right m) ->
                       do let m' = transition m
                          when (opts ^. verbose)
                               (LB.putStrLn (encode m'))

-- printGroupSums :: Plan -> IO ()
-- printGroupSums m =
--   let instanceGroups =
--         map (\gs@(i:_) ->
--                (show (i ^. inInstance ^. i1InstanceType)
--                ,(case (i ^. inInstance ^. i1Placement ^. pAvailabilityZone) of
--                    Nothing -> ""
--                    Just az -> T.unpack az)
--                ,"instance"
--                ,if isJust (i ^. inInstance ^. i1VpcId)
--                    then "vpc"
--                    else "classic"
--                ,i ^. inAccount
--                ,(length gs)))
--             (groupBy matchingInstance (sortBy comparingInstance (m ^. instances)))
--       reservedGroups =
--         map (\gs@(r:_) ->
--                (case (r ^. reReserved ^. ri1InstanceType) of
--                   Nothing -> "(unknown)"
--                   Just rType -> show rType
--                ,case (r ^. reReserved ^. ri1AvailabilityZone) of
--                   Nothing -> "(unknown)"
--                   Just az -> T.unpack az
--                ,"reserved"
--                ,if ((r ^. reReserved ^. ri1ProductDescription) `elem`
--                     [Just RIPDLinuxUNIXAmazonVPC,Just RIPDWindowsAmazonVPC])
--                    then "vpc"
--                    else "classic"
--                ,r ^. reAccount
--                ,sum (catMaybes (map (view ri1InstanceCount . view reReserved) gs))))
--             (groupBy matchingReserved
--                      (sortBy comparingReserved (m ^. reserved)))
--   in traverse_ print (sort (concat [instanceGroups,reservedGroups]))

-- printChangesPlanned :: Plan -> IO ()
-- printChangesPlanned m =
--   do traverse_ (putStrLn . T.unpack . summary)
--                (filter (not . null . view reNewInstances)
--                        (m ^. reserved))
--      traverse_ (putStrLn . T.unpack . summary)
--                (m ^. combined)

-- changeReserved :: Plan -> IO ()
-- changeReserved m =
--   traverse_ (\r ->
--                do rs <- doUpdateReserved r
--                   putStrLn (T.unpack (summary r <> " -> " <>
--                                       case rs of
--                                         (Left err) ->
--                                           (fromString . show $ err)
--                                         (Right rim) ->
--                                           maybe "(unknown)"
--                                                 id
--                                                 (rim ^.
--                                                  mrirReservedInstancesModificationId))))
--             (filter (not . null . view reNewInstances)
--                     (m ^. reserved))

-- | Transform the Plan through steps & return the new Plan.
transition :: AwsPlanTransition
transition = splitReserved . mergeReserved . matchReserved

-- | Transform Reserved + Instance = Used Reserved matching
-- on instance type, network type & availability zone.
matchReserved :: AwsPlanTransition
matchReserved =
  mergeInstances matchFn mergeFn
  where matchFn m0 m1 = m0 `appliesTo` m1 && m0 `hasCapacityFor` m1
        mergeFn r@Item{..} i = Used r [i]
        mergeFn u@Used{..} r@Item{..} = u & usedBy %~ (|> r)

-- | Match Reserved with Instances in the same offering type, network
-- type & region.
splitReserved :: AwsPlanTransition
splitReserved =
  mergeInstances matchFn mergeFn
  where matchFn m0 m1 = m0 `couldSplit` m1 && m0 `hasCapacityFor` m1
        mergeFn r@Item{..} i = Split r [i]
        mergeFn s@Split{..} i = s & splitBy %~ (|> i)

-- | Of the Reserved Instances that aren't currently being used,
-- merge RIs where conditions permit (see Amazon specs).
mergeReserved :: AwsPlanTransition
mergeReserved = undefined -- TODO WRITE THIS!

-- let (reserves,rest) = (Item (Reserved))
-- foldr over those returning (r,rejects)

-- mergeReserved (Plan os rs cs) =
--   go (groupBy matches rs) [] cs
--   where go [] rs' cs' = Plan os rs' cs'
--         go (g:gs) rs' cs' =
--           if length g > 1
--              then go gs rs' (cons (Merge g) cs')
--              else go gs (g ++ rs') cs'
--         matches r0 r1
--           | isNothing (r0 ^. reReserved ^. ri1InstanceType) ||
--               isNothing (r1 ^. reReserved ^. ri1InstanceType) = False
--         matches (Reserved _ er0 r0 [] _) (Reserved _ er1 r1 [] _) =
--           -- same end date
--           (r0 ^. ri1End == r1 ^. ri1End) &&
--           -- same instance type group (C1, M3, etc)
--           (instanceTypeDetails (r0 ^. ri1InstanceType ^?! _Just) ==
--            instanceTypeDetails (r1 ^. ri1InstanceType ^?! _Just)) &&
--           -- -- same offering type
--           (r0 ^. ri1OfferingType == r1 ^. ri1OfferingType) &&
--           -- same region
--           (er0 ^. envRegion == er1 ^. envRegion)
--         matches _ _ = False

-- | This function is an abstraction. We repeatedly need to take a
-- Reserved that we know little about & associate Instance instances
-- with it based on criteria.
--
-- Using the supplied first-argument function, find the Reserved that
-- match. From that list look for any Instance that match the
-- second-argument function. If there is a match we pass Reserved data
-- to the 3rd function which constructs a new Reserved record (which
-- we exchange for the prevous Reserved record in the Plan).
--
-- This is repeated for every Reserved in the Plan. At the end of the
-- recursion you are left with a new version of the Plan.
-- mergeInstances :: (AwsPlan -> AwsPlan -> Bool)
--                -> (AwsPlan -> AwsPlan -> AwsPlan)
--                -> AwsPlanTransition
mergeInstances matchFn mergeFn m = undefined
  -- go os rs []
  -- where go [] ys zs = Plan [] (ys ++ zs) cs
  --       go xs [] zs = Plan xs zs cs
  --       go xs (y:ys) zs =
  --         -- do we have any instances that match this reserved?
  --         case partition (matchFn y) xs of
  --           ([],_) -> go xs ys (y : zs)
  --           (hits,misses) ->
  --             -- fold instances that fit left into the reserved
  --             let (result,rejects) =
  --                   foldl (\(r,rejected) o ->
  --                            if (isInstanceTypeMatch r o)
  --                               then (mergeFn r o,rejected)
  --                               else (r,o : rejected))
  --                         (y,[])
  --                         hits
  --             in
  --                -- then on to the next reserved
  --                go (misses ++ rejects)
  --                   ys
  --                   (result : zs)

-- doUpdateReserved :: Reserved
--                  -> IO (Either Error ModifyReservedInstancesResponse)
-- doUpdateReserved r
--   | isNothing (r ^. reReserved ^. ri1ReservedInstancesId) ||
--       isNothing (r ^. reReserved ^. ri1InstanceCount) = error "WTF"
-- doUpdateReserved r =
--   do uuid <- nextRandom
--      let r' = r ^. reReserved
--          (newFactor,new) =
--            foldl (\(total,xs) os@((Instance _ _ i):_) ->
--                     (total
--                     ,(reservedInstancesConfiguration &
--                       (ricAvailabilityZone .~ i ^. i1Placement ^.
--                        pAvailabilityZone) &
--                       (ricInstanceType ?~ i ^. i1InstanceType) &
--                       (ricPlatform ?~
--                        case (i ^. i1VpcId) of
--                          Nothing -> "EC2-Classic"
--                          Just _ -> "EC2-VPC") &
--                       (ricInstanceCount ?~ length os)) :
--                      xs))
--                  (0 :: Double,[])
--                  (groupBy matchingInstance
--                           (sortBy comparingInstance (r ^. reNewInstances)))
--          -- Calculate a pad
--          oldType = r' ^. ri1InstanceType ^?! _Just
--          oldCount = r' ^. ri1InstanceCount ^?! _Just
--          (InstanceTypeDetails _ s) = instanceTypeDetails oldType
--          rFactor = instanceSizeFactor s
--          reservedFactor = rFactor * fromIntegral oldCount
--          padFactor = reservedFactor - newFactor
--          pad =
--            round (padFactor / realToFrac rFactor) :: Int
--          old =
--            case (r ^. reInstances) of
--              [] -> []
--              is@(i:_) ->
--                [(reservedInstancesConfiguration &
--                  (ricAvailabilityZone .~ r' ^. ri1AvailabilityZone) &
--                  (ricInstanceType .~ r' ^?! ri1InstanceType) &
--                  (ricPlatform ?~
--                   case (i ^. inInstance ^. i1VpcId) of
--                     Nothing -> "EC2-Classic"
--                     Just _ -> "EC2-VPC") &
--                  (ricInstanceCount ?~
--                   (length is) +
--                   pad))]
--      runAWST (r ^. reEnv)
--              (send (modifyReservedInstances &
--                     (mriReservedInstancesIds .~
--                      [r ^. reReserved ^. ri1ReservedInstancesId ^?! _Just]) &
--                     (mriClientToken ?~
--                      T.pack (toString uuid)) &
--                     (mriTargetConfigurations .~ old ++ new)))

-- doMergeReserved :: Merge
--                   -> IO (Either Error ModifyReservedInstancesResponse)
-- doMergeReserved = undefined
-- -- TODO calculate the match between two different sized reserved

-- comparingReserved :: Reserved -> Reserved -> Ordering
-- comparingReserved (Reserved a0 _ r0 _ _) (Reserved a1 _ r1 _ _) =
--   comparing id a0 a1 <>
--   comparing (view ri1InstanceType) r0 r1 <>
--   comparing (view ri1OfferingType) r0 r1 <>
--   comparing (view ri1AvailabilityZone) r0 r1 <>
--   comparing (view ri1ProductDescription) r0 r1

-- matchingReserved :: Reserved -> Reserved -> Bool
-- matchingReserved (Reserved a0 _ r0 _ _) (Reserved a1 _ r1 _ _) =
--   (a0 == a1) &&
--   (r0 ^. ri1InstanceType == r1 ^. ri1InstanceType) &&
--   (r0 ^. ri1OfferingType == r1 ^. ri1OfferingType) &&
--   (r0 ^. ri1AvailabilityZone == r1 ^. ri1AvailabilityZone) &&
--   (r0 ^. ri1ProductDescription == r1 ^. ri1ProductDescription)

-- comparingInstance :: Instance -> Instance -> Ordering
-- comparingInstance (Instance _ _ i0) (Instance _ _ i1) =
--   comparing (view i1VpcId) i0 i1 <>
--   comparing (view i1InstanceType) i0 i1 <>
--   comparing (view i1Platform) i0 i1 <>
--   comparing (view pAvailabilityZone . view i1Placement) i0 i1

-- matchingInstance :: Instance -> Instance -> Bool
-- matchingInstance (Instance _ _ i0) (Instance _ _ i1) =
--   (i0 ^. i1VpcId == i1 ^. i1VpcId) &&
--   (i0 ^. i1InstanceType == i1 ^. i1InstanceType) &&
--   (i0 ^. i1Platform == i1 ^. i1Platform) &&
--   (i0 ^. i1Placement ^. pAvailabilityZone == i1 ^. i1Placement ^.
--                                              pAvailabilityZone)
