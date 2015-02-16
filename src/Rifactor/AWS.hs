{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- Module      : Rifactor.AWS
-- Copyright   : (c) 2015 Knewton, Inc <se@knewton.com>
--               (c) 2015 Tim Dysinger <tim@dysinger.net> (contributor)
-- License     : Apache 2.0 http://opensource.org/licenses/Apache-2.0
-- Maintainer  : Tim Dysinger <tim@dysinger.net>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Rifactor.AWS where

import           BasePrelude
import           Control.Lens
import qualified Control.Monad.Trans.AWS as AWS
import           Control.Monad.Trans.AWS hiding (Empty,Env)
import qualified Data.ByteString.Char8 as B
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.AWS.Data as AWS
import qualified Network.AWS.EC2 as EC2
import           Network.AWS.EC2 hiding (Instance,Region)
import           Rifactor.Types

default (Text)

{- Amazon Environments -}

noKeysEnv :: IO AwsEnv
noKeysEnv =
  Env <$>
  AWS.getEnv
    NorthVirginia
    (FromKeys (AccessKey B.empty)
              (SecretKey B.empty)) <*>
  pure "noop"

initEnvs :: Config -> Logger -> IO [AwsEnv]
initEnvs cfg lgr =
  for [(a,r) | r <- cfg ^. regions
             , a <- cfg ^. accounts]
      (\(Account n k s,r) ->
         Env <$>
         (AWS.getEnv
            r
            (FromKeys (AccessKey (T.encodeUtf8 k))
                      (SecretKey (T.encodeUtf8 s))) <&>
          (envLogger .~ lgr)) <*>
         pure n)

{- Amazon API Queries -}

checkPendingModifications :: [AwsEnv] -> AWS ()
checkPendingModifications =
  traverse_ (\e ->
               runAWST (e ^. eEnv)
                       (do rims <-
                             view drimrReservedInstancesModifications <$>
                             send (describeReservedInstancesModifications &
                                   (drimFilters .~
                                    [filter' "status" &
                                     fValues .~
                                     [T.pack "processing"]]))
                           if null rims
                              then pure ()
                              else error "There are pending RI modifications."))

fetchFromAmazon :: [AwsEnv] -> AWS AwsPlan
fetchFromAmazon es =
  do insts <- fetchInstances es
     rsrvs <- fetchReserved es
     pure (case insts ++ rsrvs of
             [] -> Noop
             is -> Plans (map Item is))

fetchInstances :: [AwsEnv] -> AWS [AwsResource]
fetchInstances =
  liftA concat .
  traverse (\e ->
              runAWST (e ^. eEnv)
                      (view dirReservations <$>
                       send (describeInstances & di1Filters .~
                             [filter' "instance-state-name" &
                              fValues .~
                              [AWS.toText ISNRunning]])) >>=
              hoistEither >>=
              pure .
              map (Instance e) .
              concatMap (view rInstances))

fetchReserved :: [AwsEnv] -> AWS [AwsResource]
fetchReserved =
  liftA concat .
  traverse (\e ->
              runAWST (e ^. eEnv)
                      (view drirReservedInstances <$>
                       send (describeReservedInstances & driFilters .~
                             [filter' "state" &
                              fValues .~
                              [AWS.toText RISActive]])) >>=
              hoistEither >>=
              pure .
              map (Reserved e))

{- AWS Plan Queries -}

typeSet :: AwsPlan -> Set InstanceType
typeSet = foldr f Set.empty
  where f (Reserved _ _) b = b
        f (Instance _ i) b =
          Set.insert (i ^. i1InstanceType)
                     b

regionSet  :: AwsPlan -> Set Region
regionSet = foldr f Set.empty
  where f (Reserved e _) b =
          Set.insert (e ^. eEnv ^. envRegion)
                     b
        f (Instance e _) b =
          Set.insert (e ^. eEnv ^. envRegion)
                     b

zoneSet  :: AwsPlan -> Set Text
zoneSet =
  Set.fromList .
  catMaybes .
  toList .
  foldr f Set.empty
  where f (Reserved _ r) b =
          Set.insert (r ^. ri1AvailabilityZone)
                     b
        f (Instance _ i) b =
          Set.insert (i ^. i1Placement ^. pAvailabilityZone)
                     b

sumNormalizationFactor :: AwsPlan -> Maybe Float
sumNormalizationFactor = foldr f (Just 0)
  where f (Reserved _ r) b =
          liftA2 (+)
                 b
                 (liftA2 (*)
                         (fmap realToFrac (r ^. ri1InstanceCount))
                         (fmap (view insFactor)
                               (fmap find1ByType (r ^. ri1InstanceType))))
        f (Instance _ i) b =
          fmap (find1ByType (i ^. i1InstanceType) ^.
                insFactor +)
               b

sumInstanceCount :: AwsPlan -> Maybe Int
sumInstanceCount m = foldr f (Just 0) m
  where f (Instance _ _) b = fmap (+ 1) b
        f (Reserved _ r) b =
          liftA2 (+) (r ^. ri1InstanceCount) b

hasCapacityFor :: AwsPlan -> AwsPlan -> Bool
hasCapacityFor = undefined -- TODO WRITE THIS!

isInstance :: AwsPlan -> Bool
isInstance = foldr f False
  where f Instance{..} _ = True
        f _ _ = False

isReserved :: AwsPlan -> Bool
isReserved = foldr f False
  where f Reserved{..} _ = True
        f _ _ = False

foldInstance fn b m0 m1 = foldr (f fn) b m0
  where f f' x@Instance{..} z = foldr (fn x) True m1
        f f' _ z = z

foldReserved fn b m0 m1 = foldr (f fn) b m0
  where f f' x@Reserved{..} z = foldr (fn x) True m1
        f f' _ z = z

couldWorkWith :: AwsPlan -> AwsPlan -> Bool
couldWorkWith m0 m1 =
  foldReserved isInstanceMatch False m0 m1
  where isInstanceMatch (Reserved er r) (Instance ei i) _ =
          -- windows goes with windows
          (((r ^. ri1ProductDescription) `elem`
            [Just RIPDWindows,Just RIPDWindowsAmazonVPC]) ==
           ((i ^. i1Platform) ==
            Just Windows)) &&
          -- vpc goes with vpc
          (((r ^. ri1ProductDescription) `elem`
            [Just RIPDLinuxUNIXAmazonVPC,Just RIPDWindowsAmazonVPC]) ==
           isJust (i ^. i1VpcId)) &&
          -- same region
          (er ^. eEnv ^. envRegion) ==
          (ei ^. eEnv ^. envRegion)
        isInstanceMatch _ _ _ = False

worksWith :: AwsPlan -> AwsPlan -> Bool
worksWith m0 m1 =
  foldr isMatch True m0
  where isMatch a@Reserved{..} b =
          b &&
          foldr (isInstanceMatch a) True m1
        isMatch _ _ = False
        isInstanceMatch (Reserved _ r) (Instance _ i) b =
          b &&
          -- windows goes with windows
          (((r ^. ri1ProductDescription) `elem`
            [Just RIPDWindows,Just RIPDWindowsAmazonVPC]) ==
           ((i ^. i1Platform) ==
            Just Windows)) &&
          -- vpc goes with vpc
          (((r ^. ri1ProductDescription) `elem`
            [Just RIPDLinuxUNIXAmazonVPC,Just RIPDWindowsAmazonVPC]) ==
           isJust (i ^. i1VpcId)) &&
          -- same instance type
          (r ^. ri1InstanceType == i ^? i1InstanceType) &&
          -- same availability zone
          (r ^. ri1AvailabilityZone == i ^. i1Placement ^. pAvailabilityZone)
        isInstanceMatch _ _ _ = False

{- EC2 Instance Type/Group/Factor Table & Lookup -}

find1ByType :: EC2.InstanceType -> IType
find1ByType t =
  head (filter ((==) t . view insType) instanceTypes)

findByGroup :: IGroup -> [IType]
findByGroup g =
  filter ((==) g . view insGroup) instanceTypes

findByFactor :: IGroup -> Float -> [IType]
findByFactor g f =
  filter (\i -> i ^. insGroup == g && i ^. insFactor == f) instanceTypes

instanceTypes :: [IType]
instanceTypes =
  [IType C1 C1_Medium (normFactor Medium)
  ,IType C1 C1_XLarge (normFactor XLarge)
  ,IType C3 C3_2XLarge (normFactor XLarge2X)
  ,IType C3 C3_4XLarge (normFactor XLarge4X)
  ,IType C3 C3_8XLarge (normFactor XLarge8X)
  ,IType C3 C3_Large (normFactor Large)
  ,IType C3 C3_XLarge (normFactor XLarge)
  ,IType C4 C4_2XLarge (normFactor XLarge2X)
  ,IType C4 C4_4XLarge (normFactor XLarge4X)
  ,IType C4 C4_8XLarge (normFactor XLarge8X)
  ,IType C4 C4_Large (normFactor Large)
  ,IType C4 C4_XLarge (normFactor XLarge)
  ,IType CC1 CC1_4XLarge (normFactor XLarge4X)
  ,IType CC2 CC2_8XLarge (normFactor XLarge8X)
  ,IType CG1 CG1_4XLarge (normFactor XLarge4X)
  ,IType CR1 CR1_8XLarge (normFactor XLarge8X)
  ,IType G2 G2_2XLarge (normFactor XLarge2X)
  ,IType HI1 HI1_4XLarge (normFactor XLarge4X)
  ,IType HS1 HS1_8XLarge (normFactor XLarge8X)
  ,IType I2 I2_2XLarge (normFactor XLarge2X)
  ,IType I2 I2_4XLarge (normFactor XLarge4X)
  ,IType I2 I2_8XLarge (normFactor XLarge8X)
  ,IType I2 I2_XLarge (normFactor XLarge)
  ,IType M1 M1_Large (normFactor Large)
  ,IType M1 M1_Medium (normFactor Medium)
  ,IType M1 M1_Small (normFactor Small)
  ,IType M1 M1_XLarge (normFactor XLarge)
  ,IType M2 M2_2XLarge (normFactor XLarge2X)
  ,IType M2 M2_4XLarge (normFactor XLarge4X)
  ,IType M2 M2_XLarge (normFactor XLarge)
  ,IType M3 M3_2XLarge (normFactor XLarge2X)
  ,IType M3 M3_Large (normFactor Large)
  ,IType M3 M3_Medium (normFactor Medium)
  ,IType M3 M3_XLarge (normFactor XLarge)
  ,IType R3 R3_2XLarge (normFactor XLarge2X)
  ,IType R3 R3_4XLarge (normFactor XLarge4X)
  ,IType R3 R3_8XLarge (normFactor XLarge8X)
  ,IType R3 R3_Large (normFactor Large)
  ,IType R3 R3_XLarge (normFactor XLarge)
  ,IType T1 T1_Micro (normFactor Micro)
  ,IType T2 T2_Medium (normFactor Medium)
  ,IType T2 T2_Micro (normFactor Micro)
  ,IType T2 T2_Small (normFactor Small)]

normFactor :: ISize -> Float
normFactor Micro    = 0.5
normFactor Small    = 1
normFactor Medium   = 2
normFactor Large    = 4
normFactor XLarge   = 8
normFactor XLarge2X = 16
normFactor XLarge4X = 32
normFactor XLarge8X = 64

-- instance Combineable AwsReserved AwsReserved where
--   combineable a b =
--     -- same end date
--     (a ^. resResv ^. ri1End == b ^. resResv ^. ri1End) &&
--     -- same instance type group (C1, M3, etc)
--     (find1ByType (a ^. resResv ^. ri1InstanceType ^?! _Just) ^.
--      insGroup ==
--      find1ByType (b ^. resResv ^. ri1InstanceType ^?! _Just) ^.
--      insGroup) &&
--     -- same offering type
--     (a ^. resResv ^. ri1OfferingType == b ^. resResv ^. ri1OfferingType) &&
--     -- same region
--     (a ^. resEnv ^. env ^. envRegion == b ^. resEnv ^. env ^. envRegion)

-- instance Combineable AwsCombineReserved AwsReserved where
--   combineable a b =
--     all (combineable b)
--         (a ^. combine ^.. traverse)

-- instance Mergeable AwsReserved AwsInstance (Maybe AwsUsedReserved) where
--   merge a b =
--     if matchable a b
--        then Just (Used a [b])
--        else Nothing

-- instance Mergeable AwsUsedReserved AwsInstance (Maybe AwsUsedReserved) where
--   merge a b =
--     if matchable a b
--        then Just (a & usedBy %~ (|> b))
--        else Nothing

-- instance Mergeable AwsReserved AwsInstance (Maybe AwsSplitReserved) where
--   merge a b =
--     if splittable a b
--        then Just (Split a [b])
--        else Nothing

-- instance Mergeable AwsSplitReserved AwsInstance (Maybe AwsSplitReserved) where
--   merge a b =
--     if splittable a b
--        then Just (a & splitBy %~ (|> b))
--        else Nothing

-- instance Mergeable AwsUsedReserved AwsInstance (Maybe AwsSplitUsedReserved) where
--   merge a b =
--     if splittable a b
--        then Just (Split a [b])
--        else Nothing

-- instance Mergeable AwsSplitUsedReserved AwsInstance (Maybe AwsSplitUsedReserved) where
--   merge a b =
--     if splittable a b
--        then Just (a & splitBy %~ (|> b))
--        else Nothing

-- instance Mergeable AwsReserved AwsReserved (Maybe AwsCombineReserved) where
--   merge a b =
--     if combineable a b
--        then Just (Combine [a,b])
--        else Nothing

-- instance Mergeable AwsCombineReserved AwsReserved (Maybe AwsCombineReserved) where
--   merge a b =
--     if combineable a b
--        then Just (a & combine %~ (|> b))
--        else Nothing

-- instance Splittable AwsReserved AwsInstance where
--   splittable r i =
--     splittable (Split r [] :: AwsSplitReserved)
--                i

-- instance Splittable AwsUsedReserved AwsInstance where
--   splittable r i =
--     splittable (Split r [] :: AwsSplitUsedReserved)
--                i

-- instance Splittable AwsSplitReserved AwsInstance where
--   splittable r i =
--     -- windows goes with windows
--     (((r ^. split ^. resResv ^. ri1ProductDescription) `elem`
--       [Just RIPDWindows,Just RIPDWindowsAmazonVPC]) ==
--      ((i ^. insInst ^. i1Platform) ==
--       Just Windows)) &&
--     -- vpc goes with vpc
--     (((r ^. split ^. resResv ^. ri1ProductDescription) `elem`
--       [Just RIPDLinuxUNIXAmazonVPC,Just RIPDWindowsAmazonVPC]) ==
--      isJust (i ^. insInst ^. i1VpcId)) &&
--     -- same region
--     (r ^. split ^. resEnv ^. env ^. envRegion) ==
--     (i ^. insEnv ^. env ^. envRegion) &&
--     -- and also would fit into this split arrangement
--     let iGroup =
--           find1ByType (i ^. insInst ^. i1InstanceType) ^. insGroup
--         rGroup =
--           fmap (view insGroup . find1ByType)
--                (r ^. split ^. resResv ^. ri1InstanceType)
-- -- FIXME capacityTotal or Used is broken
--         Just avail =
--           liftA2 (-)
--                  (capacityTotal r)
--                  (capacityUsed r)
--         factor =
--           find1ByType (i ^. insInst ^. i1InstanceType) ^. insFactor
--     in (Just iGroup == rGroup) &&
--        factor <= avail
--        -- case fmap (factor <=) avail of
--        --   Nothing -> False
--        --   Just _ -> True

-- instance Splittable AwsSplitUsedReserved AwsInstance where
--   splittable r i =
--     -- windows goes with windows
--     (((r ^. split ^. used ^. resResv ^. ri1ProductDescription) `elem`
--       [Just RIPDWindows,Just RIPDWindowsAmazonVPC]) ==
--      ((i ^. insInst ^. i1Platform) ==
--       Just Windows)) &&
--     -- vpc goes with vpc
--     (((r ^. split ^. used ^. resResv ^. ri1ProductDescription) `elem`
--       [Just RIPDLinuxUNIXAmazonVPC,Just RIPDWindowsAmazonVPC]) ==
--      isJust (i ^. insInst ^. i1VpcId)) &&
--     -- same region
--     (r ^. split ^. used ^. resEnv ^. env ^. envRegion) ==
--     (i ^. insEnv ^. env ^. envRegion) &&
--     -- and also would fit into this split arrangement
--     let iGroup =
--           find1ByType (i ^. insInst ^. i1InstanceType) ^.
--           insGroup
--         rGroup =
--           fmap (view insGroup . find1ByType)
--                (r ^. split ^. used ^. resResv ^. ri1InstanceType)
--         avail =
--           liftA2 (-)
--                  (capacityTotal r)
--                  (capacityUsed r)
--         factor =
--           find1ByType (i ^. insInst ^. i1InstanceType) ^.
--           insFactor
--     in (Just iGroup ==
--         rGroup) &&
--        case fmap (factor <=) avail of
--          Nothing -> False
--          Just _ -> True

-- instance Matchable AwsReserved AwsInstance where
--   matchable r i =
--     -- windows goes with windows
--     (((r ^. resResv ^. ri1ProductDescription) `elem`
--       [Just RIPDWindows,Just RIPDWindowsAmazonVPC]) ==
--      ((i ^. insInst ^. i1Platform) ==
--       Just Windows)) &&
--     -- vpc goes with vpc
--     (((r ^. resResv ^. ri1ProductDescription) `elem`
--       [Just RIPDLinuxUNIXAmazonVPC,Just RIPDWindowsAmazonVPC]) ==
--      isJust (i ^. insInst ^. i1VpcId)) &&
--     -- same instance type
--     (r ^. resResv ^. ri1InstanceType) ==
--     (i ^. insInst ^? i1InstanceType) &&
--     -- same availability zone
--     (r ^. resResv ^. ri1AvailabilityZone) ==
--     (i ^. insInst ^. i1Placement ^. pAvailabilityZone)

-- instance Matchable AwsUsedReserved AwsInstance where
--   matchable r i =
--     -- fits the regular criteria
--     matchable (r ^. used)
--               i &&
--     -- and also would fit this used reserved
--     fmap (length (r ^. usedBy) <)
--          (r ^. used ^. resResv ^. ri1InstanceCount) ==
--     Just True
