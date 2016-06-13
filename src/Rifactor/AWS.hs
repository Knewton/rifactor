{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
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
import           Control.Lens hiding ((&))
import qualified Data.ByteString.Char8 as B
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Network.AWS
import           Network.AWS.Data (toText)
import qualified Network.AWS.EC2 as EC2
import           Network.AWS.EC2 hiding (Instance)
import           Rifactor.Types

default (Text)

{- Amazon Environments -}

noKeysEnv :: IO AwsEnv
noKeysEnv =
  Env <$>
  newEnv
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
         (newEnv
            r
            (FromKeys (AccessKey (T.encodeUtf8 k))
                      (SecretKey (T.encodeUtf8 s))) <&>
          (envLogger .~ lgr)) <*>
         pure n)

{- Amazon API Queries -}

reservedInstancesModifications :: [AwsEnv] -> AWS [ReservedInstancesModification]
reservedInstancesModifications =
  liftA concat .
  traverse (\e ->
              runAWS (e ^. eEnv)
                     (view drimrsReservedInstancesModifications <$>
                      send (describeReservedInstancesModifications &
                            (drimFilters .~
                             [filter' "status" &
                              fValues .~
                              [T.pack "processing"]]))))

fetchFromAmazon :: [AwsEnv] -> AWS AwsPlan
fetchFromAmazon es =
  do insts <- fetchInstances es
     rsrvs <- fetchReserved es
     pure (case insts ++ rsrvs of
             [] -> None
             is -> Plans (map Item is))

fetchInstances :: [AwsEnv] -> AWS [AwsResource]
fetchInstances =
  liftA concat .
  traverse (\e ->
              map (Instance e) .
              concatMap (view rInstances) <$>
              runAWS (e ^. eEnv)
                     (view dirsReservations <$>
                      send (describeInstances & diiFilters .~
                             [filter' "instance-state-name" &
                              fValues .~
                              [toText ISNRunning]])))

fetchReserved :: [AwsEnv] -> AWS [AwsResource]
fetchReserved =
  liftA concat .
  traverse (\e ->
              map (Reserved e) <$>
              runAWS (e ^. eEnv)
                     (view drirsReservedInstances <$>
                      send (describeReservedInstances & driFilters .~
                            [filter' "state" &
                             fValues .~
                             [toText RSActive]])))

{- AWS Plan Queries -}

instances :: AwsPlan -> [AwsResource]
instances = foldr f []
  where f Reserved{..} z = z
        f i z = snoc z i

reserved :: AwsPlan -> [AwsResource]
reserved = foldr f []
  where f Instance{..} z = z
        f i z = snoc z i

iTypeSet :: AwsPlan -> Set IType
iTypeSet = foldr f Set.empty
  where f (Reserved _ _) z = z
        f (Instance _ i) z =
          Set.insert (find1ByType (i ^. insInstanceType))
                     z

rITypeSet :: AwsPlan -> Maybe (Set IType)
rITypeSet = foldr f (Just Set.empty)
  where f (Reserved _ r) (Just z) =
          fmap (flip Set.insert z .
                find1ByType)
               (r ^. riInstanceType)
        f _ z = z

regionSet  :: AwsPlan -> Set Region
regionSet = foldr f Set.empty
  where f (Reserved e _) z =
          Set.insert (e ^. eEnv ^. envRegion)
                     z
        f (Instance e _) z =
          Set.insert (e ^. eEnv ^. envRegion)
                     z

zoneSet :: AwsPlan -> Maybe (Set Text)
zoneSet = foldr f (Just Set.empty)
  where f (Reserved _ r) (Just z) =
          fmap (flip Set.insert z)
               (r ^. riAvailabilityZone)
        f (Instance _ i) (Just z) =
          fmap (flip Set.insert z)
               (i ^. insPlacement ^. pAvailabilityZone)
        f _ z = z

instanceNormFactor :: AwsPlan -> Double
instanceNormFactor = foldr f 0
  where f (Instance _ i) z =
          z +
          find1ByType (i ^. insInstanceType) ^.
          insFactor
        f _ z = z

rInstanceNormFactor :: AwsPlan -> Maybe Double
rInstanceNormFactor = foldr f (Just 0)
  where f (Reserved _ r) (Just z) =
          fmap (z +)
               (liftA2 (*)
                       (fmap realToFrac (r ^. riInstanceCount))
                       (fmap (view insFactor)
                             (fmap find1ByType (r ^. riInstanceType))))
        f _ z = z

instanceCount :: AwsPlan -> Int
instanceCount m = foldr f 0 m
  where f (Instance _ _) z = z + 1
        f _ z = z

rInstanceCount :: AwsPlan -> Maybe Int
rInstanceCount m = foldr f (Just 0) m
  where f (Reserved _ r) (Just z) =
          fmap (+ z) (r ^. riInstanceCount)
        f _ z = z

availableNormFactor :: AwsPlan -> Maybe Double
availableNormFactor p =
  fmap (flip (-) (instanceNormFactor p))
       (rInstanceNormFactor p)

hasCapacityFor :: AwsPlan -> AwsPlan -> Bool
hasCapacityFor p0 p1 =
  let availFactor = availableNormFactor p0
      newFactor = instanceNormFactor p1
  in case fmap (flip (-) newFactor) availFactor of
       Just val -> val >= 0
       Nothing -> False

isReserved :: AwsPlan -> Bool
isReserved = foldr f False
  where f Reserved{..} _ = True
        f _ z = z

isInstance :: AwsPlan -> Bool
isInstance = not . isReserved

isItem :: AwsPlan -> Bool
isItem Item{..} = True
isItem _ = False

isUsed :: AwsPlan -> Bool
isUsed Used{..} = True
isUsed _ = False

isSplit :: AwsPlan -> Bool
isSplit Split{..} = True
isSplit _ = False

isMerge :: AwsPlan -> Bool
isMerge Merge{..} = True
isMerge _ = False

foldEachInstanceWith :: forall e r i a b f0 f1.
                        (Functor f0,Functor f1,Foldable f0,Foldable f1)
                     => (Resource e r i -> a -> b -> b)
                     -> b
                     -> f0 (Resource e r i)
                     -> f1 a
                     -> b
foldEachInstanceWith fn z p0 p1 = foldr f z p0
  where f x@Instance{..} z' = foldr (fn x) z' p1
        f _ z' = z'

foldEachReservedWith :: forall e r i a b f0 f1.
                        (Functor f0,Functor f1,Foldable f0,Foldable f1)
                     => (Resource e r i -> a -> b -> b)
                     -> b
                     -> f0 (Resource e r i)
                     -> f1 a
                     -> b
foldEachReservedWith fn z p0 p1 = foldr f z p0
  where f x@Reserved{..} z' = foldr (fn x) z' p1
        f _ z' = z'

appliesTo :: AwsPlan -> AwsPlan -> Bool
appliesTo = foldEachReservedWith (\a b -> flip (&&) (isInstanceMatch a b)) True

couldWorkWith :: AwsPlan -> AwsPlan -> Bool
couldWorkWith = foldEachReservedWith (\r i -> flip (&&) (isSplittable r i)) True

couldMerge :: AwsPlan -> AwsPlan -> Bool
couldMerge = foldEachReservedWith (\r i -> flip (&&) (isMergeable r i)) True

isInstanceMatch :: AwsResource -> AwsResource -> Bool
isInstanceMatch rs@(Reserved _ _) is@(Instance _ _) =
  sameAvailabilityZone rs is &&
  sameGroup rs is &&
  sameInstanceType rs is &&
  sameNetwork rs is &&
  samePlatform rs is
isInstanceMatch _ _ = False

isSplittable :: AwsResource -> AwsResource -> Bool
isSplittable rs@(Reserved _ _) is@(Instance _ _) =
  sameGroup rs is &&
  sameNetwork rs is &&
  samePlatform rs is &&
  sameRegion rs is
isSplittable _ _ = False

isMergeable :: AwsResource -> AwsResource -> Bool
isMergeable a@(Reserved _ r0) b@(Reserved _ r1) =
  sameAccount a b &&
  sameGroup a b &&
  sameNetwork a b &&
  samePlatform a b &&
  sameRegion a b &&
  -- not the same reserved instances
  (r0 ^. riReservedInstancesId /= r1 ^. riReservedInstancesId) &&
  -- but still the same end date
  (r0 ^. riEnd == r1 ^. riEnd) &&
  -- and the same offering type
  (r0 ^. riOfferingType == r1 ^. riOfferingType)
isMergeable _ _ = False

isVPC :: AwsResource -> Bool
isVPC (Reserved _ r) =
  (r ^. riProductDescription) `elem`
  [Just RIDLinuxUnixAmazonVPC,Just RIDWindowsAmazonVPC]
isVPC (Instance _ i) = isJust (i ^. insVPCId)

sameAccount :: AwsResource -> AwsResource -> Bool
sameAccount a b = a ^. rEnv ^. eName == b ^. rEnv ^. eName

sameRegion :: AwsResource -> AwsResource -> Bool
sameRegion a b =
  (a ^. rEnv ^. eEnv ^. envRegion == b ^. rEnv ^. eEnv ^. envRegion)

sameInstanceType :: AwsResource -> AwsResource -> Bool
sameInstanceType (Reserved _ r0) (Reserved _ r1)=
  (r0 ^. riInstanceType == r1 ^. riInstanceType)
sameInstanceType (Instance _ a) (Instance _ b) =
  (a ^. insInstanceType == b ^. insInstanceType)
sameInstanceType (Reserved _ r) (Instance _ i)=
  (r ^. riInstanceType == i ^? insInstanceType)
sameInstanceType a@Instance{..} b = sameInstanceType b a

sameAvailabilityZone :: AwsResource -> AwsResource -> Bool
sameAvailabilityZone (Reserved _ r0) (Reserved _ r1)=
  (r0 ^. riAvailabilityZone == r1 ^. riAvailabilityZone)
sameAvailabilityZone (Instance _ i0) (Instance _ i1) =
  (i0 ^. insPlacement ^. pAvailabilityZone) ==
  (i1 ^. insPlacement ^. pAvailabilityZone)
sameAvailabilityZone (Reserved _ r) (Instance _ i) =
  (r ^. riAvailabilityZone == i ^. insPlacement ^. pAvailabilityZone)
sameAvailabilityZone a@Instance{..} b = sameAvailabilityZone b a

sameGroup :: AwsResource -> AwsResource -> Bool
sameGroup (Reserved _ r0) (Reserved _ r1) =
  (find1ByType (r0 ^. riInstanceType ^?! _Just) ^. -- FIXME
   insGroup ==
   find1ByType (r1 ^. riInstanceType ^?! _Just) ^. -- FIXME
   insGroup)
sameGroup (Instance _ a) (Instance _ b) =
  (find1ByType (a ^. insInstanceType) ^.
   insGroup ==
   find1ByType (b ^. insInstanceType) ^.
   insGroup)
sameGroup (Reserved _ r) (Instance _ i) =
  fmap (view insGroup . find1ByType)
       (r ^. riInstanceType) ==
  Just (find1ByType (i ^. insInstanceType) ^.
        insGroup)
sameGroup a@Instance{..} b = sameGroup b a

sameNetwork :: AwsResource -> AwsResource -> Bool
sameNetwork (Reserved _ r0) (Reserved _ r1) =
  (((r0 ^. riProductDescription) `elem`
    [Just RIDLinuxUnixAmazonVPC,Just RIDWindowsAmazonVPC]) ==
   ((r1 ^. riProductDescription) `elem`
    [Just RIDLinuxUnixAmazonVPC,Just RIDWindowsAmazonVPC]))
sameNetwork (Instance _ i0) (Instance _ i1) =
  i0 ^. insVPCId == i1 ^. insVPCId
sameNetwork (Reserved _ r) (Instance _ i) =
  (((r ^. riProductDescription) `elem`
    [Just RIDLinuxUnixAmazonVPC,Just RIDWindowsAmazonVPC]) ==
   isJust (i ^. insVPCId))
sameNetwork a@Instance{..} b = sameNetwork b a

samePlatform :: AwsResource -> AwsResource -> Bool
samePlatform (Reserved _ r0) (Reserved _ r1) =
  (((r0 ^. riProductDescription) `elem`
    [Just RIDWindows,Just RIDWindowsAmazonVPC]) ==
   ((r1 ^. riProductDescription) `elem`
    [Just RIDWindows,Just RIDWindowsAmazonVPC]))
samePlatform (Instance _ a) (Instance _ b) =
  (a ^. insPlatform == b ^. insPlatform)
samePlatform (Reserved _ r) (Instance _ i) =
  (((r ^. riProductDescription) `elem`
    [Just RIDWindows,Just RIDWindowsAmazonVPC]) ==
   ((i ^. insPlatform) ==
    Just Windows))
samePlatform a@Instance{..} b = samePlatform b a

comparingResource :: AwsResource -> AwsResource -> Ordering
comparingResource (Reserved _ r0) (Reserved _ r1) =
  comparing (view riInstanceType) r0 r1 <>
  comparing (view riOfferingType) r0 r1 <>
  comparing (view riAvailabilityZone) r0 r1 <>
  comparing (view riProductDescription) r0 r1
comparingResource (Instance _ i0) (Instance _ i1) =
  comparing (view insVPCId) i0 i1 <>
  comparing (view insInstanceType) i0 i1 <>
  comparing (view insPlatform) i0 i1 <>
  comparing (view pAvailabilityZone . view insPlacement) i0 i1
comparingResource _ (Instance _ _) = GT
comparingResource (Instance _ _) _ = LT

matchingResource :: AwsResource -> AwsResource -> Bool
matchingResource a@(Reserved _ r0) b@(Reserved _ r1) =
  sameAvailabilityZone a b &&
  sameInstanceType a b &&
  (r0 ^. riOfferingType == r1 ^. riOfferingType) &&
  (r0 ^. riProductDescription == r1 ^. riProductDescription)
matchingResource a@(Instance _ _) b@(Instance _ _) =
  sameAvailabilityZone a b &&
  sameInstanceType a b &&
  sameNetwork a b &&
  samePlatform a b
matchingResource _ _ = False

{- EC2 Instance Type/Group/Factor Table & Lookup -}

find1ByType :: EC2.InstanceType -> IType
find1ByType t =
  head (filter ((==) t . view insType) instanceTypes)

findByGroup :: IGroup -> [IType]
findByGroup g =
  filter ((==) g . view insGroup) instanceTypes

findByFactor :: IGroup -> Double -> [IType]
findByFactor g f =
  filter (\i -> i ^. insGroup == g && i ^. insFactor == f) instanceTypes

instanceTypes :: [IType]
instanceTypes =
  sortBy (comparing (view insFactor))
         ([IType C1 C1_Medium (normFactor Medium)
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
          ,IType M4 M4_Large (normFactor Large)
          ,IType M4 M4_XLarge (normFactor XLarge)
          ,IType M4 M4_2XLarge (normFactor XLarge2X)
          ,IType M4 M4_4XLarge (normFactor XLarge4X)
          ,IType M4 M4_10XLarge (normFactor XLarge10X)
          ,IType R3 R3_2XLarge (normFactor XLarge2X)
          ,IType R3 R3_4XLarge (normFactor XLarge4X)
          ,IType R3 R3_8XLarge (normFactor XLarge8X)
          ,IType R3 R3_Large (normFactor Large)
          ,IType R3 R3_XLarge (normFactor XLarge)
          ,IType T1 T1_Micro (normFactor Micro)
          ,IType T2 T2_Large (normFactor Large)
          ,IType T2 T2_Medium (normFactor Medium)
          ,IType T2 T2_Micro (normFactor Micro)
          ,IType T2 T2_Small (normFactor Small)])

normFactor :: ISize -> Double
normFactor Micro    = 0.5
normFactor Small    = 1
normFactor Medium   = 2
normFactor Large    = 4
normFactor XLarge   = 8
normFactor XLarge2X = 16
normFactor XLarge4X = 32
normFactor XLarge8X = 64
normFactor XLarge10X = 80
