import Test.Hspec
import Sample

main :: IO ()
main = hspec $ do

  describe "something" $ do

    describe "option 1" $ do

      it "returns safe tail for option 1" $ do
        safeTail1 [] `shouldBe` ([] :: [Int])

      it "returns normal tail for option 1" $ do
        safeTail1 [1,2,3] `shouldBe` [2,3]

    describe "option 2" $ do

      it "returns safe tail for option 2" $ do
        safeTail2 [] `shouldBe` ([] :: [Int])

      it "returns normal tail for option 2" $ do
        safeTail2 [1,2,3] `shouldBe` [2,3]

    describe "option 3" $ do

      it "returns safe tail for option 3" $ do
        safeTail3 [] `shouldBe` ([] :: [Int])

      it "returns normal tail for option 3" $ do
        safeTail3 [1,2,3] `shouldBe` [2,3]

    describe "option 4" $ do

      it "returns safe tail for option 4" $ do
        safeTail4 [] `shouldBe` ([] :: [Int])

      it "returns normal tail for option 4" $ do
        safeTail4 [1,2,3] `shouldBe` [2,3]

    describe "option 5" $ do

      it "returns safe tail for option 5" $ do
        safeTail5 [] `shouldBe` ([] :: [Int])

      it "returns normal tail for option 5" $ do
        safeTail5 [1,2,3] `shouldBe` [2,3]
