{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- Module      : Main
-- Copyright   : (c) 2015 Knewton, Inc <se@knewton.com>
--               (c) 2015 Tim Dysinger <tim@dysinger.net> (contributor)
-- License     : Apache 2.0 http://opensource.org/licenses/Apache-2.0
-- Maintainer  : Tim Dysinger <tim@dysinger.net>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

import           BasePrelude
import           Control.Lens
import           Control.Monad.Trans.AWS (Env, envRegion)
import qualified Data.Text as T
import           Data.Time (UTCTime, getCurrentTime)
import qualified Network.AWS.EC2 as EC2
import           Network.AWS.EC2 hiding (Instance)
import           Rifactor.AWS
import           Rifactor.Capacity
import           Rifactor.Plan
import           Rifactor.Summary
import           Rifactor.Types
import           Test.Tasty
import           Test.Tasty.Hspec

main :: IO ()
main = tests >>= defaultMain

tests :: IO TestTree
tests =
  sequence [matchSpec, packSpec, combineSpec] >>=
  pure .
  testGroup "Tests"

matchSpec :: IO TestTree
matchSpec =
  testSpec "Matching Reserved Instances" $
  do describe "20 reserved (m2.4xlarge/us-east-1a)" $
       do context "and 20 instances (m2.4xlarge/us-east-1b)" $
            it "will not match because of the AZ" $
            do os <-
                 mkInstances 20 "us-east-1b" M2_4XLarge
               rs <-
                 mkReserved 2 "us-east-1a" 10 M2_4XLarge
               let (Model os' rs' cs') =
                     matchReserved (Model os rs [])
               length os' `shouldBe` length os
               length rs' `shouldBe` length rs
               cs' `shouldBe` empty
          context "and 20 instances (m2.4xlarge/us-east-1a)" $
            it "will match by type, network & AZ" $
            do os <-
                 mkInstances 20 "us-east-1a" M2_4XLarge
               rs <-
                 mkReserved 2 "us-east-1a" 10 M2_4XLarge
               let (Model os' rs' cs') =
                     matchReserved (Model os rs [])
               length rs' `shouldBe` length rs
               traverse_ (\r ->
                            length (r ^. reInstances) `shouldBe`
                            10)
                         rs'
               os' `shouldBe` empty
               cs' `shouldBe` empty

packSpec :: IO TestTree
packSpec =
  testSpec "Packing Reserved Instances" $
  do describe "40 reserved (m2.4xlarge/us-east-1a)" $
       context "with 20 instances (m2.4xlarge/us-east-1b)" $
       do context "and 20 instances (m2.4xlarge/us-east-1c)" $
            it "will pack all instances into reserved" $
            do os <-
                 liftM2 (++)
                        (mkInstances 20 "us-east-1b" M2_4XLarge)
                        (mkInstances 20 "us-east-1c" M2_4XLarge)
               rs <-
                 mkReserved 1 "us-east-1a" 40 M2_4XLarge
               let (Model os' rs' cs') =
                     packReserved (matchReserved (Model os rs []))
               os' `shouldBe` empty
               length rs' `shouldBe` 1
               length ((rs' !! 0) ^.
                       reInstances) `shouldBe`
                 0
               length ((rs' !! 0) ^.
                       reNewInstances) `shouldBe`
                 40
               cs' `shouldBe` empty
          context "and 20 instances (m2.2xlarge/us-east-1c)" $
            it "will pack all instances into reserved" $
            do os <-
                 liftM2 (++)
                        (mkInstances 20 "us-east-1b" M2_4XLarge)
                        (mkInstances 40 "us-east-1c" M2_2XLarge)
               rs <-
                 mkReserved 1 "us-east-1a" 40 M2_4XLarge
               let (Model os' rs' cs') =
                     packReserved (matchReserved (Model os rs []))
               os' `shouldBe` empty
               length rs' `shouldBe` 1
               length ((rs' !! 0) ^.
                       reInstances) `shouldBe`
                 0
               length ((rs' !! 0) ^.
                       reNewInstances) `shouldBe`
                 60
               cs' `shouldBe` empty
     describe "40 reserved (m2.4xlarge/us-east-1a)" $
       context "and 20 instances (m2.4xlarge/us-east-1a)" $
       do context "and 20 instances (m2.4xlarge/us-east-1c)" $
            it "will pack all instances into reserved" $
            do os <-
                 liftM2 (++)
                        (mkInstances 20 "us-east-1a" M2_4XLarge)
                        (mkInstances 20 "us-east-1c" M2_4XLarge)
               rs <-
                 mkReserved 1 "us-east-1a" 40 M2_4XLarge
               let (Model os' rs' cs') =
                     packReserved (matchReserved (Model os rs []))
               os' `shouldBe` empty
               length rs' `shouldBe` 1
               length ((rs' !! 0) ^.
                       reInstances) `shouldBe`
                 20
               length ((rs' !! 0) ^.
                       reNewInstances) `shouldBe`
                 20
               cs' `shouldBe` empty

combineSpec :: IO TestTree
combineSpec =
  testSpec "Combining Reserved Instances" $
  do describe "2x10 reserved (m2.4xlarge/us-east-1a)" $
       context "and 0 instances (m2.4xlarge/us-east-1)" $
       it "will combine reserved instances purchased at the same time" $
       do rs <-
            mkReserved 2 "us-east-1a" 10 M2_4XLarge
          let (Model os' rs' cs') =
                combineReserved (Model [] rs [])
          os' `shouldBe` empty
          rs' `shouldBe` empty
          length cs' `shouldBe` 1
          length ((cs' !! 0) ^.
                  coReserved) `shouldBe`
            2

mkReserved :: Int -> String -> Int -> InstanceType -> IO [Reserved]
mkReserved rCount az iCount itype =
  do time <- getCurrentTime
     env <- noKeysEnv
     pure (map (\_ ->
                  Reserved env (riFixture iCount az itype time) [] [])
               ([1 .. rCount] :: [Int]))

mkInstances :: Int -> String -> InstanceType -> IO [Instance]
mkInstances iCount az itype =
  do time <- getCurrentTime
     env <- noKeysEnv
     mapM (pure . Instance env . iFixture az itype time . show)
          ([1 .. iCount] :: [Int])

riFixture :: Int -> String -> InstanceType -> UTCTime -> ReservedInstances
riFixture count az itype time =
  reservedInstances &
  (ri1AvailabilityZone ?~ T.pack az) &
  (ri1InstanceCount ?~ count) &
  (ri1InstanceType ?~ itype) &
  (ri1End ?~ time) &
  (ri1InstanceTenancy ?~ Dedicated)

iFixture :: String -> InstanceType -> UTCTime -> String -> EC2.Instance
iFixture az itype time iid =
  instance' (T.pack iid)
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
            False

instance Show Env where
  show = show . view envRegion

instance Show Model where
  show = T.unpack . summary

instance Show Instance where
  show = T.unpack . summary

instance Show Reserved where
  show = T.unpack . summary

instance Show Combine where
  show = T.unpack . summary
