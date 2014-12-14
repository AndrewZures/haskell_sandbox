import Test.Hspec
import FizzBuzz

main :: IO ()
main = hspec $ do

  describe "Fizz Buzz" $ do

    it "returns fizz if n divisible by three" $ do
      FizzBuzz.match 3 `shouldBe` "fizz"

    it "returns buzz if n divisible by five" $ do
      FizzBuzz.match 5 `shouldBe` "buzz"

    it "returns fizzbuzz if n divisible by 5 and 3" $ do
      FizzBuzz.match 15 `shouldBe` "fizzbuzz"

    it "returns number as string if not divisible by 5 or 3" $ do
      FizzBuzz.match 11 `shouldBe` "11"

    it "should return [1,2,fizz,4,buzz] for first 5 num" $ do
      FizzBuzz.run [1,2,3,4,5] `shouldBe` ["1","2","fizz","4","buzz"]

    it "should return [11,'fizz',13,14,'fizzbuzz']" $ do
      FizzBuzz.run [11,12,13,14,15] `shouldBe` ["11","fizz","13","14","fizzbuzz"]
