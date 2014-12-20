import Test.Hspec
import Solver

main :: IO ()
main = hspec $ do

  describe "Euler Problem 3" $ do

    it "something" 4 do
      Solver.find_uniq_primes_for(13195) `shouldBe` [5,7,13,29]

