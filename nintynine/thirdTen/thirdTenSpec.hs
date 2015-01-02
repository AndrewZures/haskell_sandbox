import Test.Hspec
import ThirdTen

main :: IO ()
main = hspec $ do

  describe "21-30 of haskell ninety nine" $ do

    it "#21a inserts at n" $ do
      insertAt 'c' "abd" 3 `shouldBe` "abcd"

    it "#21b inserts at n" $ do
      insertAt 'X' "abcd" 2 `shouldBe` "aXbcd"

    it "#22a creates a range" $ do
      myRange 4 9 `shouldBe` [4,5,6,7,8,9]

    it "#22b creates a range" $ do
      myRange 'a' 'e' `shouldBe` "abcde"

    it "#23a takes random n items from list" $ do



