import Test.Hspec
import ThirdTen
import System.Random

main :: IO ()
main = hspec $ do

  describe "21-30 of haskell ninety nine" $ do

    it "#21a inserts at n" $ do
      insertAt 'c' "abd" 3 `shouldBe` "abcd"

    it "#21b inserts at n" $ do
      insertAt 'X' "abcd" 2 `shouldBe` "aXbcd"

    it "#22a creates a range (int)" $ do
      myRange 4 9 `shouldBe` [4,5,6,7,8,9]

    it "#22b creates a range (char)" $ do
      myRange 'a' 'e' `shouldBe` "abcde"

    it "#23a takes random n items from list" $ do
      let gen = mkStdGen 42
      randomSelect 1 6 gen 5 `shouldBe` [6,4,2,5,3]

    it "finds an element in a list (char)" $ do
      find' 'e' "abcdef" `shouldBe` ('e', "abcdf")

    it "finds an element in a list (int)" $ do
      find' 4 [5,6,7,8,9,3,4,5] `shouldBe` (4, [5,6,7,8,9,3,5])

    it "takes at an index" $ do
      takeAt' 1 [1,2,3] `shouldBe` (2, [1,3])

    it "#23a takes random n items from list" $ do
      let gen = mkStdGen 42
      randomSelect' 2 [1,2,3,4,5,6] gen `shouldBe` ([6,2], [1,3,4,5])

    it "#24 takes random n items from 1..m range" $ do
      let gen = mkStdGen 42
      randomFromRange 2 6 gen `shouldBe` ([6,2], [1,3,4,5])

