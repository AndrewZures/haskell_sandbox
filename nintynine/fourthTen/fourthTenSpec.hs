import Test.Hspec
import FourthTen

main :: IO ()
main = hspec $ do

  describe "fourth ten arithmetic" $ do

    it "#32 greatest common denominator" $ do
      FourthTen.myGcd 36 63 basic `shouldBe` 9

    it "#32 greatest common denominator" $ do
      FourthTen.myGcd 1 1 basic `shouldBe` 1

    it "#32 greatest common denominator" $ do
      FourthTen.myGcd 1071 462 basic `shouldBe` 21

    it "#32 greatest common denominator" $ do
      FourthTen.myGcd 1071 462 optimized `shouldBe` 21

    it "#33 coprime" $ do
      FourthTen.myCoprime 1071 462 optimized `shouldBe` False

    it "#33 coprime" $ do
      FourthTen.myCoprime 35 64 optimized `shouldBe` True

    -- it "#34 totient" $ do
    --   FourthTen.totient 10 optimized `shouldBe` 4

