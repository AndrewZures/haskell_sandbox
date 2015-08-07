import Test.Hspec
import Basics

main :: IO ()
main = hspec $ do

  describe "basic functions rewritten" $ do

    it "rewritten map" $ do
      Basics.myMap (+ 1) [1,2,3] `shouldBe` [2,3,4]

    it "rewritten concat" $ do
      Basics.myConcat [[1], [2,3]] `shouldBe` [1,2,3]

    it "rewritten filter" $ do
      Basics.myFilter (1 <) [1,2,3] `shouldBe` [2,3]

    it "rewritten curry filter" $ do
      Basics.curryFilter (1 <) [1,2,3] `shouldBe` [2,3]

    it "rewritten zip" $ do
      Basics.myZip ["a", "b"] [1,2] `shouldBe` [("a", 1), ("b", 2)]

    it "rewritten zipWith" $ do
      Basics.myZipWith (+) [4,5] [1,2] `shouldBe` [5,7]

    it "rewritten zip with zipWith" $ do
      Basics.myZipWith (,) [4,5] [1,2] `shouldBe` [(4,1),(5,2)]

    it "rewritten and" $ do
      Basics.myAnd [True, True, True] `shouldBe` True

    it "rewritten non decreasing list check" $ do
      Basics.nonDec [1,2,3,4,5] `shouldBe` True

    it "rewritten position" $ do
      Basics.position 3 [4,3,5,6] `shouldBe` 1

    it "rewritten position" $ do
      Basics.position 7 [4,3,5,6] `shouldBe` (-1)
