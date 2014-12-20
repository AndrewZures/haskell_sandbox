import Test.Hspec
import Solver

main :: IO ()
main = hspec $ do

  describe "Euler Problem 1 Solver" $ do

    it "stuff" $ do
      Solver.run([1..10]) `shouldBe` 23

    it "solves for 1000" $ do
      Solver.run([1..1000]) `shouldBe` 233168
