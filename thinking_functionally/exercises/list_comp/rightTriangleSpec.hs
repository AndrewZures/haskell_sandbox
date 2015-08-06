import Test.Hspec
import RightTriangle

main :: IO ()
main = hspec $ do

  describe "My List Comprehension" $ do

    it "maps over a list" $ do
      RightTriangle.find 5 `shouldBe` [(3,4,5)]

    it "determines if lists are disjoint" $ do
      RightTriangle.disjoint [1] [2] `shouldBe` True

    it "determines lists are not disjoint" $ do
      RightTriangle.disjoint [1,2] [2] `shouldBe` False

    it "determines lists are not disjoint" $ do
      RightTriangle.disjoint [1] [1,2] `shouldBe` False

    it "rewrites map as list comp" $ do
      RightTriangle.lcMap (+ 1) [1,2,3] `shouldBe` [2,3,4]

    it "rewrites filter as list comp" $ do
      RightTriangle.lcFilter (> 1) [1,2,3] `shouldBe` [2,3]

    it "rewrites concat as list comp" $ do
      RightTriangle.lcConcat [[1,2], [3]] `shouldBe` [1,2,3]
