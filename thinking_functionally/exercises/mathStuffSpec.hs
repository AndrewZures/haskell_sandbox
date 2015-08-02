import Test.Hspec
import MathStuff

main :: IO ()
main = hspec $ do

  describe "Floor" $ do

    -- it "finds the floor of 58.14" do
    --   MathStuff.floor(58.14) `shouldBe` 58

    it "finds less than value" $ do
      MathStuff.lower (-11.4) `shouldBe` -16

    it "finds greater than value" $ do
      MathStuff.upper 7 `shouldBe` 8

    it "bounds a positive Float" $ do
      MathStuff.bound 7.45 `shouldBe` (-1, 8)

    it "bounds a negative Float" $ do
      MathStuff.bound (-15.3) `shouldBe` (-16, 1)

    it "chooses a dividing number" $ do
      MathStuff.choose (1,11) `shouldBe` 6

    it "partitions" $ do
      MathStuff.shrink 2 (0, 8) `shouldBe` (0, 4)

    it "partitions 2" $ do
      MathStuff.shrink 7 (0, 8) `shouldBe` (4, 8)
