import Test.Hspec
import Solver

main :: IO ()
main = hspec $ do

  describe "Euler Problem 4" $ do

    it "finds a palindrome" $ do
      isPalindrome [9,0,0,9] `shouldBe` True
      isPalindrome [1,2,3,4] `shouldBe` False

    it "turns number into array of integers" $ do
      toArray 9009 `shouldBe` [9,0,0,9]
      toArray 1234 `shouldBe` [1,2,3,4]

    -- it "determines palindrome product for a number" $ do
    --   palindromeProd 9009 `shouldBe` k

