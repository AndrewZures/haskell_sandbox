import Test.Hspec
import Solver

main :: IO ()
main = hspec $ do

  describe "Euler Problem 9" $ do

    it "finds sum of squares of range" $ do
      isPathTriplet (4,3,5) `shouldBe` False
      isPathTriplet (3,4,5) `shouldBe` True
      isPathTriplet (3,4,6) `shouldBe` False

    it "finds expected triplet" $ do
      tripleFor 1000 `shouldBe` (200,375,425)
      tripleFor 12 `shouldBe` (3,4,5)

