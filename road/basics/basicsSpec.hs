import Test.Hspec
import Basics

main :: IO ()
main = hspec $ do

  describe "road logic basics" $ do

    it "writes divides" $ do
      divides 2 4 `shouldBe` True
      divides 3 4 `shouldBe` False

