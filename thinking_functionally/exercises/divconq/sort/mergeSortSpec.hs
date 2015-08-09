import Test.Hspec
import MergeSort

main :: IO ()
main = hspec $ do

  describe "n log n sort" $ do

    it "sorts a small list" $ do
      MergeSort.sort [4,3,1,2] `shouldBe` [1,2,3,4]

    it "sorts a larger list" $ do
      MergeSort.sort [5,6,3,1,2,9,7,4,8] `shouldBe` [1,2,3,4,5,6,7,8,9]
