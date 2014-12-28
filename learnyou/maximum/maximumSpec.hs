import Test.Hspec
import Maximum

main :: IO ()
main = hspec $ do

  describe "maximum via recursion" $ do

    it "finds max" $ do
      Maximum.run([1,2,4,3]) `shouldBe` 4

    it "Finds max again" $ do
      Maximum.run([1,2,4,3,10,-1]) `shouldBe` 10

    it "finds max" $ do
      Maximum.run2([1,2,4,3]) `shouldBe` 4

    it "Finds max again" $ do
      Maximum.run2([1,2,4,3,10,-1]) `shouldBe` 10


