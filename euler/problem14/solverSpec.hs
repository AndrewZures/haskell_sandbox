import Test.Hspec
import Solver

main :: IO ()
main = hspec $ do

  describe "Euler Problem 9" $ do

    it "hi" $ do
      collatzCnt 1 `shouldBe` 1
      collatzCnt 13 `shouldBe` 10

    it "stuff" $ do
      maxCollatzCnt 1000000 `shouldBe` 10
