import Test.Hspec
import FirstTen

main :: IO ()
main = hspec $ do

  describe "first ten of haskell 99 problems" $ do

    it "#1 finds last element in a list" $ do
      FirstTen.findLast [4,3,5,2,6] `shouldBe` 6

    it "#2a finds last element in a int list" $ do
      FirstTen.findSecondToLast [4,3,5,2,6] `shouldBe` 2

    it "#2b finds last element in a char list" $ do
      FirstTen.findSecondToLast "qwertyu" `shouldBe` 'y'

    it "#3 find nth element of list" $ do
      FirstTen.elementAt "qwert" 2 `shouldBe` 'w'

    it "#4a find number of elements in a list" $ do
      FirstTen.numElements "qwerty" `shouldBe` 6

    it "#4b find number of elements in a list" $ do
      FirstTen.numElements [123, 456, 789] `shouldBe` 3

    it "#5 reverses a list" $ do
      FirstTen.myReverse "qwerty" `shouldBe` "ytrewq"

    it "#6a finds palindrome" $ do
      FirstTen.isPalindrome "racecar" `shouldBe` True

    it "#6b finds palindrome" $ do
      FirstTen.isPalindrome "borfob" `shouldBe` False

    it "#6c finds palindrome" $ do
      FirstTen.isPalindrome "traccart" `shouldBe` True

    it "#6d finds palindrome of numbers" $ do
      FirstTen.isPalindrome [1,2,3,2,1] `shouldBe` True

    it "#6e returns false for empty list" $ do
      FirstTen.isPalindrome [1] `shouldBe` True

    it "#7 flattens a list" $ do
      FirstTen.myFlatten [[1,2,3],[4,5,6]] `shouldBe` [1,2,3,4,5,6]

    it "#8 flattens a list" $ do
      FirstTen.myFlatten2 [[1,2,3],[4,5,6]] `shouldBe` [1,2,3,4,5,6]






