{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Lens
import qualified Data.Text as T
import           Network.AWS.EC2
import           Rifactor.Plan
import           Rifactor.Types
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.TH

case_PerfectMatch :: Assertion
case_PerfectMatch =
  True @=?
  all match
      (matchActiveReserved
         (concat [mkInstances 20 "us-east-1a" M2_4XLarge
                 ,mkReserved 2 "us-east-1a" 10 M2_4XLarge]))
  where match (UsedReserved{..}) = True
        match _ = False

case_PartialMatch :: Assertion
case_PartialMatch =
  1 @=?
  length (filter match
                 (matchActiveReserved
                    (concat [mkInstances 19 "us-east-1a" M2_4XLarge
                            ,mkReserved 2 "us-east-1a" 10 M2_4XLarge])))
  where match (PartialReserved{..}) = True
        match _ = False

riFixture :: Int -> String -> InstanceType -> ReservedInstances
riFixture count az itype =
  reservedInstances &
  (ri1AvailabilityZone ?~ T.pack az) &
  (ri1InstanceCount ?~ count) &
  (ri1InstanceType ?~ itype) &
  (ri1InstanceTenancy ?~ Dedicated)

iFixture :: String -> InstanceType -> String -> Instance
iFixture az itype iid =
  (instance' (T.pack iid)
             (T.pack "ami-fake0")
             (instanceState 16 ISNRunning)
             42
             itype
             undefined
             (placement &
              (pAvailabilityZone ?~ T.pack az))
             (monitoring &
              (mState ?~ MSDisabled))
             X8664
             InstanceStore
             Hvm
             Xen
             False)

mkReserved :: Int -> String -> Int -> InstanceType -> [Resource]
mkReserved rCount az iCount itype =
  map (\_ ->
         UnmatchedReserved undefined
                           (riFixture iCount az itype))
      ([1 .. rCount] :: [Int])

mkInstances :: Int -> String -> InstanceType -> [Resource]
mkInstances count az itype =
  map (\iid ->
         UnmatchedInstance (iFixture az itype (show iid)))
      ([1 .. count] :: [Int])

tg :: TestTree
tg = $(testGroupGenerator)

main :: IO ()
main = defaultMain tg
