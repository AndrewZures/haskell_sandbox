import Prelude hiding ((||))
import Test.Hspec
import Sample

main :: IO ()
main = hspec $ do

  describe "something" $ do

    describe "option 1" $ do

      it "evaluates to False option 1" $ do
        False `or1` False `shouldBe` False

      it "evaluates to True option 1" $ do
        False `or1` True `shouldBe` True

    describe "option 2" $ do

      it "evaluates to False option 2" $ do
        False `or2` False `shouldBe` False

      it "evaluates to True option 2" $ do
        False `or2` True `shouldBe` True

    describe "option 3" $ do

      it "evaluates to False option 3" $ do
        False `or3` False `shouldBe` False

      it "evaluates to True option 3" $ do
        False `or3` True `shouldBe` True

    describe "option 4" $ do

      it "evaluates to False option 4" $ do
        False `or4` False `shouldBe` False

      it "evaluates to True option 4" $ do
        False `or4` True `shouldBe` True

    describe "option 5" $ do

      it "evaluates to False option 5" $ do
        False `or5` False `shouldBe` False

      it "evaluates to True option 5" $ do
        False `or5` True `shouldBe` True
