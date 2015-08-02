import Test.Hspec
import MathStuff

main :: IO ()
main = hspec $ do

  describe "Floor" $ do

    it "finds leq for float integer" $ do
      MathStuff.leq 3 11.12 `shouldBe` True

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

    it "finds" $ do
      MathStuff.found (1,2) `shouldBe` True

    it "finds floor of a float" $ do
      MathStuff.floor 34.25 `shouldBe` 34

    it "finds floor of a negative float" $ do
      MathStuff.floor (-12.34) `shouldBe` (-13)

