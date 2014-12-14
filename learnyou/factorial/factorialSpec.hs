import Test.Hspec
import Factorial

main :: IO ()
main = hspec $ do

  describe "Factorial" $ do

    it "returns 1 for factorial(0)" $ do
      Factorial.run(0) `shouldBe` 1

    it "returns 6 for factorial(3)" $ do
      Factorial.run(3) `shouldBe` 6

    it "returns 3,628,800 for factorial(10)" $ do
      Factorial.run(10) `shouldBe` 3628800




