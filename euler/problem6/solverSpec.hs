import Test.Hspec
import Solver

main :: IO ()
main = hspec $ do

  describe "Euler Problem 5" $ do

    it "finds sum of squares of range" $ do
      sumOfSquares [1..10] `shouldBe` 385

    it "finds smallest divisible up to 20" $ do
      squareOfSums [1..10] `shouldBe` 3025

    it "finds difference of sumOfSq and sqOfSum" $ do
      diff [1..10] `shouldBe` 2640
      diff [1..100] `shouldBe` 2640


