import Test.Hspec
import Sample

main :: IO ()
main = hspec $ do

  describe "something" $ do

    describe "option 1" $ do

      it "evaluates to False option 1" $ do
        remove1 1 [1,2,3,4] `shouldBe` [1,3,4]

      it "correctly evaluates mystery function" $ do
        mystery 3 [1,2,3,4,5,6,7] `shouldBe` [1,2,3,4,4,5,6,7]
