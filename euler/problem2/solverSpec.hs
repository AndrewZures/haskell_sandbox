import Test.Hspec
import Solver

main :: IO ()
main = hspec $ do

  describe "Euler Problem 2" $ do

    it "gets all fibs under a given number" $ do
      Solver.dp_find_fibs(100) `shouldBe` [0,1,1,2,3,5,8,13,21,34,55,89]

    it "sums all even fibs under 100" $ do
      Solver.sum_evens([1,2,4,5,6]) `shouldBe` 12

    it "sum all even fibs under given number" $ do
      Solver.run(100) `shouldBe` 44

    it "sums all even fibs under 4,000,000" $ do
      Solver.run(4000000) `shouldBe` 4613732




