import Test.Hspec
import Misc

main :: IO ()
main = hspec $ do

  describe "miscellaneous functions" $ do

    it "finds disjoint of ascending lists - true" $ do
      Misc.ascDisjoint [3,4,5] [9,11,12] `shouldBe` True

    it "finds disjoint of ascending lists - false" $ do
      Misc.ascDisjoint [3,4,5] [4,11,12] `shouldBe` False
