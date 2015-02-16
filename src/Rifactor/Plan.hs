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

-- printChangesPlanned :: AwsPlan -> IO ()
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
  where matchFn r i = r `appliesTo` i && r `hasCapacityFor` i
        mergeFn r@Item{..} i = Used r [i]
        mergeFn u@Used{..} i = u & usedBy %~ (|> i)

-- | Match Reserved with Instances in the same offering type, network
-- type & region.
splitReserved :: AwsPlanTransition
splitReserved =
  mergeInstances matchFn mergeFn
  where matchFn r i = r `couldSplit` i && r `hasCapacityFor` i
        mergeFn r@Item{..} i = Split r [i]
        mergeFn r@Used{..} i = Split r [i]
        mergeFn s@Split{..} i = s & splitBy %~ (|> i)

-- | Of the Reserved Instances that aren't currently being used,
-- merge RIs where conditions permit (see Amazon specs).
mergeReserved :: AwsPlanTransition
mergeReserved (Plans ps) =
  let (rs,rest) =
        partition (\x ->
                     isReserved x &&
                     (isItem x || isMerge x))
                  ps
  in go rs rest
  where go [] zs = Plans zs
        go (x:xs) zs =
          let (z,xs') =
                foldr (\r' (r,rs) ->
                         if (couldMerge r r')
                            then (mergeFn r r',rs)
                            else (r,r' : rs))
                      (x,[])
                      xs
          in go xs' (z : zs)
        mergeFn r0@Item{..} r1 = Merge [r0,r1]
        mergeFn m@Merge{..} r = m & merged %~ (|> r)
mergeReserved p = p

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
mergeInstances :: (AwsPlan -> AwsPlan -> Bool)
               -> (AwsPlan -> AwsPlan -> AwsPlan)
               -> AwsPlanTransition
mergeInstances matchFn mergeFn (Plans ps) =
  let (rs,is) = partition isReserved ps
  in go rs is []
  where go [] ys zs = Plans (ys ++ zs)
        go xs [] zs = Plans (xs ++ zs)
        go (x:xs) ys zs =
          let (z,ys') =
                foldr (\i (r,is) ->
                         if (matchFn r i)
                            then (mergeFn r i,is)
                            else (r,i : is))
                      (x,[])
                      ys
          in go xs ys' (z : zs)
mergeInstances _ _ p = p

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
