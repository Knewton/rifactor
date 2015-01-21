import Data.List (sort)
import Rifactor.Plan
import Rifactor.Types
import Test.Tasty
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
                          sort (reverse list)]]
