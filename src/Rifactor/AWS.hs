{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

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
import           Control.Monad.Trans.AWS hiding (Env)
import qualified Data.ByteString.Char8 as B
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Network.AWS.Data (toText)
import qualified Network.AWS.EC2 as EC2
import           Network.AWS.EC2 hiding (Instance)
import           Rifactor.Types

default (Text)

noKeysEnv :: IO AwsEnv
noKeysEnv =
  Env <$>
  AWS.getEnv
    AWS.NorthVirginia
    (AWS.FromKeys (AWS.AccessKey B.empty)
                  (AWS.SecretKey B.empty)) <*>
  pure "noop"

initEnvs :: Config -> AWS.Logger -> IO [AwsEnv]
initEnvs cfg lgr =
  for [(a,r) | r <- cfg ^. regions
             , a <- cfg ^. accounts]
      (\(Account n k s,r) ->
         Env <$>
         (AWS.getEnv
            r
            (AWS.FromKeys (AWS.AccessKey (T.encodeUtf8 k))
                          (AWS.SecretKey (T.encodeUtf8 s))) <&>
          (AWS.envLogger .~ lgr)) <*>
         pure n)

checkPendingModifications :: [AwsEnv] -> AWS ()
checkPendingModifications =
  traverse_ (\e ->
               runAWST (e ^. env)
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

fetchFromAmazon :: [AwsEnv] -> AWS AwsModel
fetchFromAmazon es =
  Model <$> fetchInstances es <*> fetchReserved es <*>
  pure [] <*>
  pure [] <*>
  pure [] <*>
  pure []

fetchInstances :: [AwsEnv] -> AWS [AwsInstance]
fetchInstances =
  liftA concat .
  traverse (\e ->
              runAWST (e ^. env)
                      (view dirReservations <$>
                       send (describeInstances & di1Filters .~
                             [filter' "instance-state-name" &
                              fValues .~
                              [toText ISNRunning]])) >>=
              hoistEither >>=
              pure .
              map (Instance e) .
              concatMap (view rInstances))

fetchReserved :: [AwsEnv] -> AWS [AwsReserved]
fetchReserved =
  liftA concat .
  traverse (\e ->
              runAWST (e ^. env)
                      (view drirReservedInstances <$>
                       send (describeReservedInstances & driFilters .~
                             [filter' "state" &
                              fValues .~
                              [toText RISActive]])) >>=
              hoistEither >>=
              pure .
              map (\r -> Reserved e r))

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

instance Combineable AwsReserved AwsReserved where
  combineable a b =
    -- same end date
    (a ^. resResv ^. ri1End == b ^. resResv ^. ri1End) &&
    -- same instance type group (C1, M3, etc)
    (find1ByType (a ^. resResv ^. ri1InstanceType ^?! _Just) ^.
     insGroup ==
     find1ByType (b ^. resResv ^. ri1InstanceType ^?! _Just) ^.
     insGroup) &&
    -- same offering type
    (a ^. resResv ^. ri1OfferingType == b ^. resResv ^. ri1OfferingType) &&
    -- same region
    (a ^. resEnv ^. env ^. envRegion == b ^. resEnv ^. env ^. envRegion)

instance Combineable AwsCombineReserved AwsReserved where
  combineable a b =
    all (combineable b)
        (a ^. combine ^.. traverse)

instance Mergeable AwsReserved AwsInstance (Maybe AwsUsedReserved) where
  merge a b =
    if matchable a b
       then Just (Used a [b])
       else Nothing

instance Mergeable AwsUsedReserved AwsInstance (Maybe AwsUsedReserved) where
  merge a b =
    if matchable a b
       then Just (a & usedBy %~ (|> b))
       else Nothing

instance Mergeable AwsReserved AwsInstance (Maybe AwsSplitReserved) where
  merge a b =
    if splittable a b
       then Just (Split a [b])
       else Nothing

instance Mergeable AwsSplitReserved AwsInstance (Maybe AwsSplitReserved) where
  merge a b =
    if splittable a b
       then Just (a & splitBy %~ (|> b))
       else Nothing

instance Mergeable AwsUsedReserved AwsInstance (Maybe AwsSplitUsedReserved) where
  merge a b =
    if splittable a b
       then Just (Split a [b])
       else Nothing

instance Mergeable AwsSplitUsedReserved AwsInstance (Maybe AwsSplitUsedReserved) where
  merge a b =
    if splittable a b
       then Just (a & splitBy %~ (|> b))
       else Nothing

instance Mergeable AwsReserved AwsReserved (Maybe AwsCombineReserved) where
  merge a b =
    if combineable a b
       then Just (Combine [a,b])
       else Nothing

instance Mergeable AwsCombineReserved AwsReserved (Maybe AwsCombineReserved) where
  merge a b =
    if combineable a b
       then Just (a & combine %~ (|> b))
       else Nothing

instance Splittable AwsReserved AwsInstance where
  splittable r i =
    splittable (Split r [] :: AwsSplitReserved)
               i

instance Splittable AwsUsedReserved AwsInstance where
  splittable r i =
    splittable (Split r [] :: AwsSplitUsedReserved)
               i

instance Splittable AwsSplitReserved AwsInstance where
  splittable r i =
    -- windows goes with windows
    (((r ^. split ^. resResv ^. ri1ProductDescription) `elem`
      [Just RIPDWindows,Just RIPDWindowsAmazonVPC]) ==
     ((i ^. insInst ^. i1Platform) ==
      Just Windows)) &&
    -- vpc goes with vpc
    (((r ^. split ^. resResv ^. ri1ProductDescription) `elem`
      [Just RIPDLinuxUNIXAmazonVPC,Just RIPDWindowsAmazonVPC]) ==
     isJust (i ^. insInst ^. i1VpcId)) &&
    -- same region
    (r ^. split ^. resEnv ^. env ^. envRegion) ==
    (i ^. insEnv ^. env ^. envRegion) &&
    -- and also would fit into this split arrangement
    let iGroup =
          find1ByType (i ^. insInst ^. i1InstanceType) ^. insGroup
        rGroup =
          fmap (view insGroup . find1ByType)
               (r ^. split ^. resResv ^. ri1InstanceType)
-- FIXME capacityTotal or Used is broken
        Just avail =
          liftA2 (-)
                 (capacityTotal r)
                 (capacityUsed r)
        factor =
          find1ByType (i ^. insInst ^. i1InstanceType) ^. insFactor
    in (Just iGroup == rGroup) &&
       factor <= avail
       -- case fmap (factor <=) avail of
       --   Nothing -> False
       --   Just _ -> True

instance Splittable AwsSplitUsedReserved AwsInstance where
  splittable r i =
    -- windows goes with windows
    (((r ^. split ^. used ^. resResv ^. ri1ProductDescription) `elem`
      [Just RIPDWindows,Just RIPDWindowsAmazonVPC]) ==
     ((i ^. insInst ^. i1Platform) ==
      Just Windows)) &&
    -- vpc goes with vpc
    (((r ^. split ^. used ^. resResv ^. ri1ProductDescription) `elem`
      [Just RIPDLinuxUNIXAmazonVPC,Just RIPDWindowsAmazonVPC]) ==
     isJust (i ^. insInst ^. i1VpcId)) &&
    -- same region
    (r ^. split ^. used ^. resEnv ^. env ^. envRegion) ==
    (i ^. insEnv ^. env ^. envRegion) &&
    -- and also would fit into this split arrangement
    let iGroup =
          find1ByType (i ^. insInst ^. i1InstanceType) ^.
          insGroup
        rGroup =
          fmap (view insGroup . find1ByType)
               (r ^. split ^. used ^. resResv ^. ri1InstanceType)
        avail =
          liftA2 (-)
                 (capacityTotal r)
                 (capacityUsed r)
        factor =
          find1ByType (i ^. insInst ^. i1InstanceType) ^.
          insFactor
    in (Just iGroup ==
        rGroup) &&
       case fmap (factor <=) avail of
         Nothing -> False
         Just _ -> True

instance Matchable AwsReserved AwsInstance where
  matchable r i =
    -- windows goes with windows
    (((r ^. resResv ^. ri1ProductDescription) `elem`
      [Just RIPDWindows,Just RIPDWindowsAmazonVPC]) ==
     ((i ^. insInst ^. i1Platform) ==
      Just Windows)) &&
    -- vpc goes with vpc
    (((r ^. resResv ^. ri1ProductDescription) `elem`
      [Just RIPDLinuxUNIXAmazonVPC,Just RIPDWindowsAmazonVPC]) ==
     isJust (i ^. insInst ^. i1VpcId)) &&
    -- same instance type
    (r ^. resResv ^. ri1InstanceType) ==
    (i ^. insInst ^? i1InstanceType) &&
    -- same availability zone
    (r ^. resResv ^. ri1AvailabilityZone) ==
    (i ^. insInst ^. i1Placement ^. pAvailabilityZone)

instance Matchable AwsUsedReserved AwsInstance where
  matchable r i =
    -- fits the regular criteria
    matchable (r ^. used)
              i &&
    -- and also would fit this used reserved
    fmap (length (r ^. usedBy) <)
         (r ^. used ^. resResv ^. ri1InstanceCount) ==
    Just True

instance Capacity AwsReserved where
  capacityUsed _ = Nothing
  capacityTotal r =
    let count =
          fmap realToFrac (r ^. resResv ^. ri1InstanceCount)
        norm =
          fmap (view insFactor . find1ByType)
               (r ^. resResv ^. ri1InstanceType)
    in liftA2 (*) count norm

instance Capacity AwsUsedReserved where
  capacityUsed u =
    fmap ((*) (realToFrac (length (u ^. usedBy))) .
          view insFactor)
         (fmap find1ByType (u ^. used ^. resResv ^. ri1InstanceType))
  capacityTotal u = capacityTotal (u ^. used)

instance Capacity AwsSplitReserved where
  capacityUsed s =
    let new =
          foldl (\a i ->
                   a +
                   (find1ByType (i ^. insInst ^. i1InstanceType) ^.
                    insFactor))
                (0 :: Float)
                (s ^. splitBy)
    in fmap (new +) (capacityUsed (s ^. split))
  capacityTotal s = capacityTotal (s ^. split)

instance Capacity AwsSplitUsedReserved where
  capacityUsed s =
    let new =
          foldl (\a i ->
                   a +
                   (find1ByType (i ^. insInst ^. i1InstanceType) ^.
                    insFactor))
                (0 :: Float)
                (s ^. splitBy)
    in fmap (new +) (capacityUsed (s ^. split))
  capacityTotal s = capacityTotal (s ^. split)
