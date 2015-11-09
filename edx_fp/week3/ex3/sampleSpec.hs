import Prelude hiding ((||))
import Test.Hspec
import Sample

main :: IO ()
main = hspec $ do

  describe "something" $ do

    describe "option 1" $ do

      it "evaluates to False option 1" $ do
        True `and1` False `shouldBe` False

      it "evaluates to True option 1" $ do
        True `and1` True `shouldBe` True

    describe "option 2" $ do

      it "evaluates to False option 2" $ do
        True `and2` False `shouldBe` False

      it "evaluates to True option 2" $ do
        True `and2` True `shouldBe` True

    describe "option 3" $ do

      it "evaluates to False option 3" $ do
        True `and3` False `shouldBe` False

      it "evaluates to True option 3" $ do
        True `and3` True `shouldBe` True

    describe "option 4" $ do

      it "evaluates to False option 4" $ do
        True `and4` False `shouldBe` False

      it "evaluates to True option 4" $ do
        True `and4` True `shouldBe` True
