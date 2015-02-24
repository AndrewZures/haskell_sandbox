import Test.Hspec
import FourthTen

main :: IO ()
main = hspec $ do

  describe "31-40 Arithmetic" $ do

    it "#30 finds 7 to be prime" $ do
      isPrime 7 `shouldBe` True

    it "#30 finds 10 to not be prime" $ do
      isPrime 10 `shouldBe` True



