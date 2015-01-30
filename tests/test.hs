{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- Module      : Main
-- Copyright   : (c) 2015 Knewton, Inc <se@knewton.com>
--               (c) 2015 Tim Dysinger <tim@dysinger.net> (contributor)
-- License     : Apache 2.0 http://opensource.org/licenses/Apache-2.0
-- Maintainer  : Tim Dysinger <tim@dysinger.net>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

import           Control.Applicative
import           Control.Lens
import           Data.Time
import qualified Data.Text as T
import           Network.AWS.EC2
import           Rifactor.Plan
import           Rifactor.Types
import           Test.Tasty
import           Test.Tasty.Hspec

main :: IO ()
main = tests >>= defaultMain

tests :: IO TestTree
tests =
  sequence [matchSpec,moveSpec,splitSpec,combineSpec,resizeSpec] >>=
  pure .
  testGroup "Tests"

matchSpec :: IO TestTree
matchSpec =
  testSpec "Matching" $
  describe "ReservedInstances With Instances" $
  context "with 20 reserved (m2.4xlarge/us-east-1a)" $
  do context "and 20 instances (m2.4xlarge/us-east-1a)" $
       do it "will match by type, network & AZ" $
            do reserved <-
                 mkReserved 2 "us-east-1a" 10 M2_4XLarge
               onDemand <-
                 mkInstances 20 "us-east-1a" M2_4XLarge
               let (usedReserved,restOfOnDemand) =
                     match (reserved,onDemand)
               all isUsedReserved usedReserved `shouldBe`
                 True
               restOfOnDemand `shouldBe` empty
     context "and 20 instances (m2.4xlarge/us-east-1b)" $
       do it "will not match by instance type alone" $
            do reserved <-
                 mkReserved 2 "us-east-1a" 10 M2_4XLarge
               onDemand <-
                 mkInstances 20 "us-east-1b" M2_4XLarge
               let (reserved',restOfOnDemand) =
                     match (reserved,onDemand)
               all isReserved reserved' `shouldBe`
                 True
               length restOfOnDemand `shouldBe` 20

moveSpec :: IO TestTree
moveSpec =
  testSpec "Moving" $
  describe "Ununused ReservedInstances" $
  context "with 20 reserved (m2.4xlarge/us-east-1a)" $
  context "and 20 instances (m2.4xlarge/us-east-1b)" $
  do it "will match by instance type & move" $
       do reserved <-
            mkReserved 2 "us-east-1a" 10 M2_4XLarge
          onDemand <-
            mkInstances 20 "us-east-1b" M2_4XLarge
          let (moveReserved,restOfOnDemand) =
                move (reserved,onDemand)
          all isMoveReserved moveReserved `shouldBe`
            True
          all (\r ->
                 length (r ^. reInstances) ==
                 10)
              (moveReserved) `shouldBe`
            True
          restOfOnDemand `shouldBe` empty

splitSpec :: IO TestTree
splitSpec =
  testSpec "Splitting" $
  describe "Used ReservedInstances (That Have Capacity)" $
  context "with 40 reserved m2.4xlarge/us-east-1a" $
  context "and 20 instances m2.4xlarge/us-east-1a" $
  context "and 20 instances m2.4xlarge/us-east-1b" $
  do it "will match all instances & split" $
       do usedInstances <-
            mkInstances 20 "us-east-1a" M2_4XLarge
          usedReserved <-
            mkUsedReserved 1 "us-east-1a" 40 M2_4XLarge usedInstances
          wrongAzInstances <-
            mkInstances 20 "us-east-1b" M2_4XLarge
          let ((splitReserved:restOfReserved),restOfOnDemand) =
                split (usedReserved,wrongAzInstances)
          isSplitReserved splitReserved `shouldBe` True
          length (splitReserved ^. reInstances) `shouldBe`
            10
          length (splitReserved ^. reNewInstances) `shouldBe`
            10
          restOfReserved `shouldBe` empty
          restOfOnDemand `shouldBe` empty

combineSpec :: IO TestTree
combineSpec =
  testSpec "Combining" $
  describe "ReservedInstances (That Are No Longer Used)" $
  context "with 10 reserved m2.4xlarge/us-east-1a" $
  context "and 10 reserved m2.4xlarge/us-east-1b" $
  context "and 0 instances m2.4xlarge/us-east-1" $
  do it "will combined reserved instances that match" $
       pending

resizeSpec :: IO TestTree
resizeSpec =
  testSpec "Resizing" $
  describe "ReservedInstances (That Are No Longer Used)" $
  context "with 100 reserved m2.4xlarge/us-east-1a" $
  context "and 0 instances m2.4xlarge/us-east-1" $
  context "and 120 instances m2.2xlarge/us-east-1" $
  do it "will resize reserved instances to match" $
       pending

mkUsedReserved :: Int -> String -> Int -> InstanceType -> [OnDemand] -> IO [Reserved]
mkUsedReserved rCount az iCount itype xs =
  do time <- getCurrentTime
     pure (map (\_ ->
                  UsedReserved undefined
                               (riFixture iCount az itype time)
                               (map (view odInstance) xs))
               ([1 .. rCount] :: [Int]))

mkReserved :: Int -> String -> Int -> InstanceType -> IO [Reserved]
mkReserved rCount az iCount itype =
  do time <- getCurrentTime
     pure (map (\_ ->
                  Reserved undefined (riFixture iCount az itype time))
               ([1 .. rCount] :: [Int]))

mkInstances :: Int -> String -> InstanceType -> IO [OnDemand]
mkInstances iCount az itype =
  mapM (\instanceNum ->
          do time <- getCurrentTime
             pure (OnDemand (iFixture az itype time (show instanceNum))))
       ([1 .. iCount] :: [Int])

riFixture :: Int -> String -> InstanceType -> UTCTime -> ReservedInstances
riFixture count az itype _time =
  reservedInstances &
  (ri1AvailabilityZone ?~ T.pack az) &
  (ri1InstanceCount ?~ count) &
  (ri1InstanceType ?~ itype) &
  (ri1InstanceTenancy ?~ Dedicated)

iFixture :: String -> InstanceType -> UTCTime -> String -> Instance
iFixture az itype time iid =
  (instance' (T.pack iid)
             (T.pack "ami-fake0")
             (instanceState 16 ISNRunning)
             42
             itype
             time
             (placement &
              (pAvailabilityZone ?~ T.pack az))
             (monitoring &
              (mState ?~ MSDisabled))
             X8664
             InstanceStore
             Hvm
             Xen
             False)
