import Test.Hspec
import Solver

main :: IO ()
main = hspec $ do

  describe "Euler Problem 9" $ do

    it "hi" $ do
      factorCount 3 `shouldBe` 2

    it "stuff" $ do
      -- hey 2 `shouldBe` 3
      -- hey 4 `shouldBe` 6
      hey 6 `shouldBe` 28
      -- hey 500 `shouldBe` 28

