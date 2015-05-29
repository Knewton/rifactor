{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- Module      : Main
-- Copyright   : (c) 2015 Knewton, Inc <se@knewton.com>
--               (c) 2015 Tim Dysinger <tim@dysinger.net> (contributor)
-- License     : Apache 2.0 http://opensource.org/licenses/Apache-2.0
-- Maintainer  : Tim Dysinger <tim@dysinger.net>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

import           BasePrelude hiding ((&))
import           Control.Lens
import           Control.Monad.Trans.AWS (Env, envRegion)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime, getCurrentTime, utctDay)
import           Data.Time.Calendar (addDays)
import qualified Network.AWS as AWS
import qualified Network.AWS.Data as AWS
import qualified Network.AWS.EC2 as EC2
import           Network.AWS.EC2 hiding (Instance,Region)
import           Rifactor.AWS
import           Rifactor.Plan
import           Rifactor.Types
import           Test.Tasty
import           Test.Tasty.Hspec

default (Text)

main :: IO ()
main = tests >>= defaultMain

tests :: IO TestTree
tests =
  sequence [matchSpec,mergeSpec,splitSpec] >>=
  pure .
  testGroup "Tests"

matchSpec :: IO TestTree
matchSpec =
  testSpec "Matching Reserved Instances" $
  do describe "10 reserved (m2.4xlarge/us-east-1a)" $
       do context "and 10 instances (m2.4xlarge/us-east-1a)" $
            do it "will match by type, network & AZ" $
                 do (r:[]) <-
                      mkReserved 1 "us-east-1a" 10 M2_4XLarge
                    is <-
                      mkInstances 10 "us-east-1a" M2_4XLarge
                    let rsItem = Item r
                        isItems = map Item is
                        (Plans ps) =
                          matchReserved (Plans (rsItem : isItems))
                    length ps `shouldBe` 1
                    all isUsed ps `shouldBe`
                      True
                    let (Used rsItem' isItems') = head ps
                    rsItem' `shouldBe` rsItem
                    isItems' `shouldMatchList` isItems
          context "and 10 instances (m2.4xlarge/us-east-1b)" $
            do it "will NOT match because of the different AZ" $
                 do rs <-
                      mkReserved 1 "us-east-1a" 10 M2_4XLarge
                    is <-
                      mkInstances 10 "us-east-1b" M2_4XLarge
                    let ps = map Item (is ++ rs)
                        (Plans ps') =
                          matchReserved (Plans ps)
                    ps' `shouldMatchList` ps
          context "and 10 instances (m2.2xlarge/us-east-1a)" $
            do it "will NOT match because of the different instance type" $
                 do rs <-
                      mkReserved 1 "us-east-1a" 10 M2_4XLarge
                    is <-
                      mkInstances 10 "us-east-1a" M2_2XLarge
                    let ps = map Item (is ++ rs)
                        (Plans ps') =
                          matchReserved (Plans ps)
                    ps' `shouldMatchList` ps

splitSpec :: IO TestTree
splitSpec =
  testSpec "Spliting Reserved Instances" $
  do describe "40 reserved (m2.4xlarge/us-east-1a)" $
       context "with 20 instances (m2.4xlarge/us-east-1b)" $
       do context "and 20 instances (m2.4xlarge/us-east-1c)" $
            it "will split all 40 instances out of an unused reserved" $
            do (r:[]) <-
                 mkReserved 1 "us-east-1a" 40 M2_4XLarge
               is <-
                 liftM2 (++)
                        (mkInstances 20 "us-east-1b" M2_4XLarge)
                        (mkInstances 20 "us-east-1c" M2_4XLarge)
               let rsItem = Item r
                   isItems = map Item is
                   (Plans ps) =
                     splitReserved (matchReserved (Plans (rsItem : isItems)))
               length ps `shouldBe` 1
               all isSplit ps `shouldBe`
                 True
               let (Split rsItem' isItems') = head ps
               rsItem' `shouldBe` rsItem
               isItems' `shouldMatchList` isItems
          context "and 40 instances (m2.2xlarge/us-east-1c)" $
            it "will split all 60 instances out of an unused reserved" $
            do (r:[]) <-
                 mkReserved 1 "us-east-1a" 40 M2_4XLarge
               is <-
                 liftM2 (++)
                        (mkInstances 20 "us-east-1b" M2_4XLarge)
                        (mkInstances 40 "us-east-1c" M2_2XLarge)
               let rsItem = Item r
                   isItems = map Item is
                   (Plans ps) =
                     splitReserved (matchReserved (Plans (rsItem : isItems)))
               length ps `shouldBe` 1
               all isSplit ps `shouldBe`
                 True
               let (Split rsItem' isItems') = head ps
               rsItem' `shouldBe` rsItem
               isItems' `shouldMatchList` isItems
     describe "40 reserved (m2.4xlarge/us-east-1a)" $
       context "and 20 instances (m2.4xlarge/us-east-1a)" $
       do context "and 20 instances (m2.4xlarge/us-east-1c)" $
            it "will split 20 instances out of a partially used reserved" $
            do (r:[]) <-
                 mkReserved 1 "us-east-1a" 40 M2_4XLarge
               is0 <-
                 (mkInstances 20 "us-east-1a" M2_4XLarge)
               is1 <-
                 (mkInstances 20 "us-east-1c" M2_4XLarge)
               let rsItem = Item r
                   is0Items = map Item is0
                   is1Items = map Item is1
                   mg@(Plans ps) =
                     matchReserved
                       (Plans (rsItem :
                               (is0Items ++ is1Items)))
               any isItem ps `shouldBe`
                 True
               any isUsed ps `shouldBe`
                 True
               let (Plans ps') = splitReserved mg
               length ps' `shouldBe` 1
               all isSplit ps' `shouldBe`
                 True
               let (Split (Used rsItem' is0Items') is1Items') = head ps'
               rsItem' `shouldBe` rsItem
               is0Items' `shouldMatchList` is0Items
               is1Items' `shouldMatchList` is1Items

mergeSpec :: IO TestTree
mergeSpec =
  testSpec "Merging Reserved Instances" $
  do describe "2x10 reserved (m2.4xlarge/us-east-1a)" $
       context "and 0 instances (m2.4xlarge/us-east-1)" $
       it "will merge reserved instances purchased at the same time" $
       example $
       do rs <-
            mkReserved 2 "us-east-1a" 10 M2_4XLarge
          let items = map Item rs
              (Plans ps) =
                mergeReserved (Plans items)
          ps `shouldMatchList`
            [Merge items]
     describe "1x10 reserved (m2.4xlarge/us-east-1a)" $
       context "and another purchased later (m2.4xlarge/us-east-1a)" $
       it "will NOT merge reserved instances purchased at different times" $
       example $
       do rs0 <-
            mkReserved 2 "us-east-1a" 10 M2_4XLarge
          rs1 <-
            mkReserved 2 "us-east-1a" 10 M2_4XLarge
          today <- getCurrentTime
          let oldReserved = map Item rs0
              tomorrow =
                today {utctDay =
                         addDays 1 (utctDay today)}
              aYearFromTomorrow =
                today {utctDay =
                         addDays 366 (utctDay tomorrow)}
              rs1' =
                map (\r ->
                       r &
                       (rReserved %~ ri1Start ?~ tomorrow) &
                       (rReserved %~ ri1End ?~ aYearFromTomorrow))
                    rs1
              newReserved = map Item rs1'
              (Plans ps) =
                mergeReserved (Plans (newReserved ++ oldReserved))
          length ps `shouldBe` 2
          all isMerge ps `shouldBe`
            True

mkReserved :: Int -> Text -> Int -> EC2.InstanceType -> IO [AwsResource]
mkReserved rCount az iCount itype =
  do time <- getCurrentTime
     e <- noKeysEnv
     pure (map (\rid ->
                  Reserved e (riFixture iCount az itype time (AWS.toText rid)))
               ([1 .. rCount] :: [Int]))

mkInstances :: Int -> Text -> EC2.InstanceType -> IO [AwsResource]
mkInstances iCount az itype =
  do time <- getCurrentTime
     e <- noKeysEnv
     mapM (pure . Instance e . iFixture az itype time . T.pack . show)
          ([1 .. iCount] :: [Int])

riFixture :: Int
          -> Text
          -> EC2.InstanceType
          -> UTCTime
          -> Text
          -> EC2.ReservedInstances
riFixture count az itype time rid =
  reservedInstances &
  (ri1ReservedInstancesId ?~ rid) &
  (ri1AvailabilityZone ?~ az) &
  (ri1InstanceCount ?~ count) &
  (ri1InstanceType ?~ itype) &
  (ri1End ?~ time) &
  (ri1InstanceTenancy ?~ Dedicated)

iFixture :: Text -> EC2.InstanceType -> UTCTime -> Text -> EC2.Instance
iFixture az itype time iid =
  instance' iid
            (T.pack "ami-fake0")
            (instanceState 16 ISNRunning)
            42
            itype
            time
            (placement &
             (pAvailabilityZone ?~ az))
            (monitoring &
             (mState ?~ MSDisabled))
            X8664
            InstanceStore
            Hvm
            Xen
            False
