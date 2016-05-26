import Test.Hspec
import PrimeGenerator

import Data.Array

main :: IO ()
main = hspec $ do

  describe "Prime Generator" $ do

    it "generates primes" $ do
      primes 10 `shouldBe` [2,3,5,7]

    it "sieves the array" $ do
      let arr = genBaseArray 5
      let result = elems $ snd $ sieve (2,arr)
      result `shouldBe` [2,3,0,5]

    -- it "sieves the array" $ do
    --   primes 1000 `shouldBe` [1,2,3,0,5]

    it "gen base array" $ do
      let expected = array(2,10) [(i,i) | i <- [2..10]]
      genBaseArray 10 `shouldBe` expected

    it "generates list or remove" $ do
        genRemList 2 6 `shouldBe` [(4,0),(6,0)]
        genRemList 3 12 `shouldBe` [(6,0),(9,0),(12,0)]

    -- it "sieves the array" $ do
    --   sumPrimes 2000000 `shouldBe` 1
