import Test.Hspec
import Solver

main :: IO ()
main = hspec $ do

  describe "Euler Problem 5" $ do

    it "finds smallest divisible up to 10" $ do
      smallDivisible [2..10] `shouldBe` Just 2520

    it "finds smallest divisible up to 20" $ do
      smallDivisible [2..20] `shouldBe` Just 232792560

    it "finds numbers divisible by all nums in a range" $ do
      isDivisibleByAll [2,3] 6 `shouldBe` True
      isDivisibleByAll [2,3] 7 `shouldBe` False
      isDivisibleByAll [2,3] 12 `shouldBe` True
      isDivisibleByAll [2..10] 2520 `shouldBe` True
