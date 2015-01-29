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
  do sMatch <- specMatch
     sMove <- specMove
     return (testGroup "Tests" [sMatch, sMove])

specMatch :: IO TestTree
specMatch =
  testSpec "Match Reserved" $
  describe "match" $
  do it "will match reserved/on-demand by type, network & AZ" $
       do reserved <-
            mkReserved 2 "us-east-1a" 10 M2_4XLarge
          onDemand <-
            mkInstances 20 "us-east-1a" M2_4XLarge
          let (reserved',onDemand') =
                match (reserved,onDemand)
          all isUsed reserved' `shouldBe`
            True
          onDemand' `shouldBe` []
     it "will not match reserved/on-demand by just instance type" $
       do reserved <-
            mkReserved 2 "us-east-1a" 10 M2_4XLarge
          onDemand <-
            mkInstances 20 "us-east-1b" M2_4XLarge
          let (reserved',onDemand') =
                match (reserved,onDemand)
          all isNotUsed reserved' `shouldBe`
            True
          length onDemand' `shouldBe` 20

specMove :: IO TestTree
specMove =
  testSpec "Move Reserved" $
  describe "move" $
  do it "will match reserved with on-demand by instance type alone" $
       do reserved <-
            mkReserved 2 "us-east-1a" 10 M2_4XLarge
          onDemand <-
            mkInstances 20 "us-east-1b" M2_4XLarge
          let (reserved',onDemand') =
                move (reserved,onDemand)
          all isUsed reserved' `shouldBe`
            True
          onDemand' `shouldBe` []

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

isNotUsed :: Reserved -> Bool
isNotUsed (Reserved{..}) = True
isNotUsed _ = True

isUsed :: Reserved -> Bool
isUsed (UsedReserved{..}) = True
isUsed _ = True
