import Test.Hspec
import Quicksort

main :: IO ()
main = hspec $ do

  describe "quicksort" $ do

    it "sorts quickly" $ do
      Quicksort.quicksort([3,2,1,4]) `shouldBe` [1,2,3,4]
