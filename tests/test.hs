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

import           BasePrelude
import           Control.Lens
import           Control.Monad.Trans.AWS (Env, envRegion)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime, getCurrentTime, utctDay)
import           Data.Time.Calendar (addDays)
import qualified Network.AWS as AWS
import           Network.AWS.Data (toText)
import qualified Network.AWS.EC2 as EC2
import           Network.AWS.EC2 hiding (InstanceType,Instance,ReservedInstances)
import           Rifactor.AWS
import           Rifactor.Execute
import           Rifactor.Types
import           Test.Tasty
import           Test.Tasty.Hspec

default (Text)

main :: IO ()
main = tests >>= defaultMain

tests :: IO TestTree
tests =
  sequence [] >>=
  pure .
  testGroup "Tests"

-- matchSpec :: IO TestTree
-- matchSpec =
--   testSpec "Matching Reserved Instances" $
--   do describe "20 reserved (m2.4xlarge/us-east-1a)" $
--        do context "and 20 instances (m2.4xlarge/us-east-1a)" $
--             do it "will match by type, network & AZ" $
--                  do os@(o0:o1:_) <-
--                       mkInstances 20 "us-east-1a" M2_4XLarge
--                     rs@(r:_) <-
--                       mkReserved 2 "us-east-1a" 10 M2_4XLarge
--                     -- test some underlying match/merge functions 1st
--                     r ^. resResv ^. ri1InstanceCount `shouldBe`
--                       Just 10
--                     matchable r o0 `shouldBe`
--                       True
--                     let (Just merge0) =
--                           merge r o0
--                     merge0 `shouldBe`
--                       Used r [o0]
--                     let (Just merge1) =
--                           merge merge0 o1
--                     merge1 `shouldBe`
--                       Used r [o0,o1]
--                     -- now test our matchReserved higher level fn
--                     let m =
--                           matchReserved (mkAwsModel os rs)
--                     length (m ^. insts) `shouldBe`
--                       0
--                     length (m ^. resvs) `shouldBe`
--                       0
--                     length (m ^. usedResvs) `shouldBe`
--                       length rs
--                     traverse_ (\x -> length x `shouldBe` 10)
--                       (m ^. usedResvs ^.. traverse . usedBy)
--           context "and 20 instances (m2.4xlarge/us-east-1b)" $
--             do it "will not match because of the different AZ" $
--                  do os <-
--                       mkInstances 20 "us-east-1b" M2_4XLarge
--                     rs <-
--                       mkReserved 2 "us-east-1a" 10 M2_4XLarge
--                     let m =
--                           matchReserved (mkAwsModel os rs)
--                     length (m ^. insts) `shouldBe`
--                       length os
--                     length (m ^. resvs) `shouldBe`
--                       length rs
--           context "and 20 instances (m2.2xlarge/us-east-1a)" $
--             do it "will not match because of the different instance type" $
--                  do os <-
--                       mkInstances 20 "us-east-1a" M2_2XLarge
--                     rs <-
--                       mkReserved 2 "us-east-1a" 10 M2_4XLarge
--                     let m =
--                           matchReserved (mkAwsModel os rs)
--                     length (m ^. insts) `shouldBe`
--                       length os
--                     length (m ^. resvs) `shouldBe`
--                       length rs

-- splitSpec :: IO TestTree
-- splitSpec =
--   testSpec "Spliting Reserved Instances" $
--   do describe "40 reserved (m2.4xlarge/us-east-1a)" $
--        context "with 20 instances (m2.4xlarge/us-east-1b)" $
--        do context "and 20 instances (m2.4xlarge/us-east-1c)" $
--             it "will split 20 instances on top of a used reserved" $
--             do os@(o0:o1:_) <-
--                  liftM2 (++)
--                         (mkInstances 20 "us-east-1b" M2_4XLarge)
--                         (mkInstances 20 "us-east-1c" M2_4XLarge)
--                rs@(r:_) <-
--                  mkReserved 1 "us-east-1a" 40 M2_4XLarge
--                -- test some underlying match/merge functions 1st
--                splittable r o0 `shouldBe` True
--                let (Just merge0) = merge r o0
--                merge0 `shouldBe` Split r [o0]
--                let (Just merge1) = merge merge0 o1
--                merge1 `shouldBe` Split r [o0,o1]
--                -- -- now test our matchReserved higher level fn
--                -- let m =
--                --       splitReserved (mkAwsModel os rs)
--                -- (m ^. insts) `shouldBe` empty
--                -- length (m ^. resvs) `shouldBe` 1
--                -- traverse_ (\x -> length x `shouldBe` 40)
--                --   (m ^. usedResvs ^.. traverse . usedBy)

--      --      context "and 20 instances (m2.2xlarge/us-east-1c)" $
--      --        it "will split all instances into reserved" $
--      --        do os <-
--      --             liftM2 (++)
--      --                    (mkInstances 20 "us-east-1b" M2_4XLarge)
--      --                    (mkInstances 40 "us-east-1c" M2_2XLarge)
--      --           rs <-
--      --             mkReserved 1 "us-east-1a" 40 M2_4XLarge
--      --           let (Model os' rs' cs') =
--      --                 splitReserved (matchReserved (Model os rs []))
--      --           os' `shouldBe` empty
--      --           length rs' `shouldBe` 1
--      --           length ((rs' !! 0) ^.
--      --                   reInstances) `shouldBe`
--      --             0
--      --           length ((rs' !! 0) ^.
--      --                   reNewInstances) `shouldBe`
--      --             60
--      --           cs' `shouldBe` empty
--      -- describe "40 reserved (m2.4xlarge/us-east-1a)" $
--      --   context "and 20 instances (m2.4xlarge/us-east-1a)" $
--      --   do context "and 20 instances (m2.4xlarge/us-east-1c)" $
--      --        it "will split all instances into reserved" $
--      --        do os <-
--      --             liftM2 (++)
--      --                    (mkInstances 20 "us-east-1a" M2_4XLarge)
--      --                    (mkInstances 20 "us-east-1c" M2_4XLarge)
--      --           rs <-
--      --             mkReserved 1 "us-east-1a" 40 M2_4XLarge
--      --           let (Model os' rs' cs') =
--      --                 splitReserved (matchReserved (Model os rs []))
--      --           os' `shouldBe` empty
--      --           length rs' `shouldBe` 1
--      --           length ((rs' !! 0) ^.
--      --                   reInstances) `shouldBe`
--      --             20
--      --           length ((rs' !! 0) ^.
--      --                   reNewInstances) `shouldBe`
--      --             20
--      --           cs' `shouldBe` empty

-- combineSpec :: IO TestTree
-- combineSpec =
--   testSpec "Combining Reserved Instances" $
--   do describe "2x10 reserved (m2.4xlarge/us-east-1a)" $
--        context "and 0 instances (m2.4xlarge/us-east-1)" $
--        it "will combine reserved instances purchased at the same time" $
--        example $
--        do (r0:r1:[]) <-
--             mkReserved 2 "us-east-1a" 10 M2_4XLarge
--           combineable r0 r1 `shouldBe` True
--           merge r0 r1 `shouldBe` Just (Combine [r0,r1])
--           let m =
--                 combineReserved (mkAwsModel [] [r0,r1])
--           (m ^. resvs) `shouldBe` empty
--           (m ^. insts) `shouldBe` empty
--           length (m ^. combineResvs) `shouldBe` 1
--           traverse_ (\c -> length c `shouldBe` 2)
--             (m ^. combineResvs ^.. traverse . combine)
--      describe "1x10 reserved (m2.4xlarge/us-east-1a)" $
--        context "and another purchased later (m2.4xlarge/us-east-1a)" $
--        it "will NOT combine reserved instances purchased at different times" $
--        example $
--        do rs0 <-
--             mkReserved 2 "us-east-1a" 10 M2_4XLarge
--           rs1 <-
--             mkReserved 2 "us-east-1a" 10 M2_4XLarge
--           today <- getCurrentTime
--           let tomorrow = today { utctDay = addDays 1 (utctDay today) }
--               aYearFromTomorrow = today { utctDay = addDays 366 (utctDay tomorrow) }
--           let rs1' = map (\r -> r & (resResv %~ ri1Start ?~ tomorrow)
--                                   & (resResv %~ ri1End ?~ aYearFromTomorrow)) rs1
--           let m =
--                 combineReserved (mkAwsModel [] (rs0++rs1'))
--           (m ^. resvs) `shouldBe` empty
--           (m ^. insts) `shouldBe` empty
--           length (m ^. combineResvs) `shouldBe` 2
--           traverse_ (\c -> length c `shouldBe` 2)
--             (m ^. combineResvs ^.. traverse . combine)

mkReserved :: Int -> Text -> Int -> EC2.InstanceType -> IO [AwsResource]
mkReserved rCount az iCount itype =
  do time <- getCurrentTime
     e <- noKeysEnv
     pure (map (\_ ->
                  Reserved e (riFixture iCount az itype time))
               ([1 .. rCount] :: [Int]))

mkInstances :: Int -> Text -> EC2.InstanceType -> IO [AwsResource]
mkInstances iCount az itype =
  do time <- getCurrentTime
     e <- noKeysEnv
     mapM (pure . Instance e . iFixture az itype time . T.pack . show)
          ([1 .. iCount] :: [Int])

riFixture :: Int -> Text -> EC2.InstanceType -> UTCTime -> EC2.ReservedInstances
riFixture count az itype time =
  reservedInstances &
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
