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
import           Control.Lens hiding ((&))
import qualified Control.Monad.Trans.AWS as AWS
import           Control.Monad.Trans.AWS hiding (Env, sourceFile)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Conduit (($$))
import           Data.Conduit.Attoparsec (sinkParser)
import           Data.Conduit.Binary (sourceFile)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import           Data.UUID.V4
import qualified Network.AWS.Data as AWS
import qualified Network.AWS.EC2 as EC2
import           Network.AWS.EC2 hiding (Error,Instance,Region)
import           Rifactor.AWS
import           Rifactor.Report
import           Rifactor.Types
import           System.IO (stdout)
import qualified Text.PrettyPrint.ANSI.Leijen as ANSI

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
                      (reservedInstancesModifications envs)
            case pending of
              Left err -> print err >> exitFailure
              Right p ->
                do when (not . null $ p)
                        (error "There are pending RI modifications.")
                   fetch <-
                     runAWST (noEnv ^. eEnv)
                             (fetchFromAmazon envs)
                   case fetch of
                     (Left err) -> print err >> exitFailure
                     (Right p') ->
                       do let (Plans ps) = transition p'
                              changes = filter (\x ->
                                                 (isSplit x ||
                                                  isMerge x)) ps
                          forM_ changes
                                (\x ->
                                   do ANSI.putDoc
                                        (report x ANSI.<>
                                         ANSI.linebreak <>
                                         ANSI.linebreak)
                                      when (not (opts ^. dryRun))
                                           (do when (isSplit x)
                                                    (do s <- doUpdateReserved x
                                                        case s of
                                                          Left err ->
                                                            ANSI.putDoc
                                                              (ANSI.hang 2
                                                                         (ANSI.indent 2 $
                                                                          ANSI.text $
                                                                          show err) ANSI.<>
                                                               ANSI.linebreak ANSI.<>
                                                               ANSI.linebreak)
                                                          Right rs ->
                                                            case rs ^.
                                                                 mrirReservedInstancesModificationId of
                                                              Nothing ->
                                                                ANSI.putDoc
                                                                  (ANSI.hang 2
                                                                             (ANSI.indent
                                                                                2
                                                                                ("OK" ANSI.<>
                                                                                 ANSI.linebreak ANSI.<>
                                                                                 ANSI.linebreak)))
                                                              Just id' ->
                                                                ANSI.putDoc
                                                                  (ANSI.hang 2
                                                                             (ANSI.indent 2
                                                                                          (ANSI.text (T.unpack id')) ANSI.<>
                                                                              ANSI.linebreak ANSI.<>
                                                                              ANSI.linebreak)))
                                               when (isMerge x)
                                                    (do m <- doMergeReserved x
                                                        case m of
                                                          Left err ->
                                                            ANSI.putDoc
                                                              (ANSI.hang 2
                                                                         (ANSI.indent 2 $
                                                                          ANSI.text $
                                                                          show err) ANSI.<>
                                                               ANSI.linebreak ANSI.<>
                                                               ANSI.linebreak)
                                                          Right rs ->
                                                            case rs ^.
                                                                 mrirReservedInstancesModificationId of
                                                              Nothing ->
                                                                ANSI.putDoc
                                                                  (ANSI.hang 2
                                                                             (ANSI.indent
                                                                                2
                                                                                ("OK" ANSI.<>
                                                                                 ANSI.linebreak ANSI.<>
                                                                                 ANSI.linebreak)))
                                                              Just id' ->
                                                                ANSI.putDoc
                                                                  (ANSI.hang 2
                                                                             (ANSI.indent 2
                                                                                          (ANSI.text (T.unpack id')) ANSI.<>
                                                                              ANSI.linebreak ANSI.<>
                                                                              ANSI.linebreak)))))

-- | Transform the Plan through steps & return the new Plan.
transition :: AwsPlanTransition
transition = splitReserved . mergeReserved . matchReserved

-- | Transform Reserved + Instance = Used Reserved matching
-- on instance type, network type & availability zone.
matchReserved :: AwsPlanTransition
matchReserved =
  mergeInstances matchFn mergeFn
  where matchFn a b = a `appliesTo` b && a `hasCapacityFor` b
        mergeFn a@Item{..} b = Used a [b]
        mergeFn u@Used{..} b = u & usedBy %~ (|> b)
        mergeFn a b = Plans [a,b] -- NOT NEEDED BUT LESS WARNINGS

-- | Match Reserved with Instances in the same offering type, network
-- type & region.
splitReserved :: AwsPlanTransition
splitReserved =
  mergeInstances matchFn mergeFn
  where matchFn a b = a `couldWorkWith` b && a `hasCapacityFor` b
        mergeFn a@Item{..} b = Split a [b]
        mergeFn a@Used{..} b = Split a [b]
        mergeFn a@Split{..} b = a & splitBy %~ (|> b)
        mergeFn a b = Plans [a,b] -- NOT NEEDED BUT LESS WARNINGS

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
        mergeFn a@Item{..} b = Merge [a,b]
        mergeFn a@Merge{..} b = a & merged %~ (|> b)
        mergeFn a b = Plans [a,b] -- NOT NEEDED BUT LESS WARNINGS
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

mkConfig :: [AwsResource] -> Maybe ReservedInstancesConfiguration
mkConfig is@(Instance _ i:_) =
          Just (mkConfig' (i ^. i1Placement ^. pAvailabilityZone)
                        (i ^. i1InstanceType)
                        (isJust (i ^. i1VpcId))
                        (length is))
mkConfig _ = Nothing

mkConfig' :: Maybe Text
          -> InstanceType
          -> Bool
          -> Int
          -> ReservedInstancesConfiguration
mkConfig' az iType isVpc count =
          reservedInstancesConfiguration &
          (ricAvailabilityZone .~ az) &
          (ricInstanceType ?~ iType) &
          (ricPlatform ?~
           if isVpc
              then "EC2-VPC"
              else "EC2-Classic") &
          (ricInstanceCount ?~ count)

foldPadding :: Maybe Text
            -> Bool
            -> Double
            -> [IType]
            -> ModifyReservedInstances
            -> ModifyReservedInstances
foldPadding _ _ _ [] rq = rq
foldPadding _ _ a _ rq | a <= 0.0 = rq
foldPadding az v a (it:its) rq =
  if a `mod'`
     (it ^. insFactor) /=
     0.0
     then foldPadding az v a its rq
     else let count =
                a `div'`
                (it ^. insFactor) :: Int
              avail' =
                a -
                (realToFrac count *
                 (it ^. insFactor))
          in foldPadding
               az
               v
               avail'
               its
               (rq & mriTargetConfigurations %~
                (|> (mkConfig' az (it ^. insType) v count)))

foldConfigs :: [AwsResource]
            -> ModifyReservedInstances
            -> Maybe ModifyReservedInstances
foldConfigs is rq =
  foldr f
        (Just rq)
        (groupBy matchingResource (sortBy comparingResource is))
  where f is' rq' =
          liftA2 (\r c -> r & mriTargetConfigurations %~ (|> c))
                 rq'
                 (mkConfig is')

doUpdateReserved :: AwsPlan -> IO (Either Error ModifyReservedInstancesResponse)
doUpdateReserved s@Split{..} =
  do uuid <- nextRandom
     let rs@(Reserved _ r) = head (reserved s) -- FIXME
         rq =
           modifyReservedInstances &
           (mriReservedInstancesIds .~
            [r ^. ri1ReservedInstancesId ^?! _Just]) &
           (mriClientToken ?~
            T.pack (UUID.toString uuid))
         rq' =
           foldConfigs (instances s)
                       rq
         maybeAvail = availableNormFactor s
         cs =
           reverse (findByGroup
                      (find1ByType
                         ((r ^. ri1InstanceType) ^?!
                          _Just) ^.
                       insGroup)) -- FIXME
         az = r ^. ri1AvailabilityZone
         v = isVPC rs
         rq'' =
           liftA2 (\a b ->
                     foldPadding az v a cs b)
                  maybeAvail
                  rq'
     case rq'' of
       Just rq''' ->
         runAWST (rs ^. rEnv ^. eEnv)
                 (send rq''')
       Nothing -> error "nothing"

doMergeReserved :: AwsPlan -> IO (Either Error ModifyReservedInstancesResponse)
doMergeReserved m@Merge{..} =
  do uuid <- nextRandom
     let rs@(h@(Reserved e r):_t) = reserved m
         rq =
           modifyReservedInstances &
           (mriReservedInstancesIds .~
            (map (\(Reserved _ r') -> r' ^. ri1ReservedInstancesId ^?! _Just) rs)) &
           (mriClientToken ?~
            T.pack (UUID.toString uuid))
         maybeAvail = availableNormFactor m
         cs =
           reverse (findByGroup
                      (find1ByType
                         ((r ^. ri1InstanceType) ^?!
                          _Just) ^.
                       insGroup)) -- FIXME
         az = r ^. ri1AvailabilityZone
         v = isVPC h
     case fmap (\a -> foldPadding az v a cs rq) maybeAvail of
       Just rq'' ->
         runAWST (e ^. eEnv)
                 (send rq'')
       Nothing -> error "nothing"
