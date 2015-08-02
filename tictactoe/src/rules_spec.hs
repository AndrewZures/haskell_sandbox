import Test.Hspec
import Rules

main :: IO ()
main = hspec $ do

  describe "scores a horizontal rows" $ do

    it "finds first horizontal row win" $ do
      expect(add 1) `shouldBe` 2
