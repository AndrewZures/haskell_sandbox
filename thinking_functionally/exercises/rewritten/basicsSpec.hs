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

