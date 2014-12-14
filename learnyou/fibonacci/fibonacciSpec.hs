import Test.Hspec
import Fibonacci

main :: IO ()
main = hspec $ do

  describe "Fibonacci Bad Runtime" $ do

    it "find fibonacci of index 3" $ do
      Fibonacci.find(3) `shouldBe` 2

    it "finds fibonacci of index 9" $ do
      Fibonacci.find(9) `shouldBe` 34

    it "find the fib for index 200" $ do
      Fibonacci.find(25) `shouldBe` 75025


  describe "DP Fibonacci" $ do

    it "finds fibonacci of index 3" $ do
      Fibonacci.dp_find(3) `shouldBe` 2

    it "finds fibonacci of index 9" $ do
      Fibonacci.dp_find(9) `shouldBe` 34

    it "find the fib for index 200" $ do
      Fibonacci.dp_find(25) `shouldBe` 75025





