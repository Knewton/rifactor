import Data.List (sort)
import Rifactor.Plan
import Rifactor.Types
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck

main :: IO ()
main =
  defaultMain $
  testGroup "Tests"
            [testGroup "(checked by SmallCheck)"
                       [testProperty "sort == sort . reverse" $
                        \list ->
                          sort (list :: [Int]) ==
                          sort (reverse list)]
            ,testGroup "Unit tests"
                       [testCase "List comparison (different length)" $
                        [1,2,3] `compare`
                        [1,2] @?=
                        GT
                       ,testCase "List comparison (same length)" $
                        [1,2,3] `compare`
                        [1,2,2] @?=
                        GT]]
