import Test.Hspec
import Repeat

main :: IO ()
main = hspec $ do

  describe "repeater" $ do

    it "repeats" $ do
      Repeat.run 3 'a' `shouldBe` ['a','a','a']

    it "zips with" $ do
      Repeat.zipWith' (+) [1,2,3] [4,5,6] `shouldBe` [5,7,9]

    it "filters" $ do
      Repeat.filter' (> 3) [2,3,4] `shouldBe` [4]

    it "flips" $ do
      Repeat.flip' 2 3 1 `shouldBe` Repeat.flip' 1 3 2
