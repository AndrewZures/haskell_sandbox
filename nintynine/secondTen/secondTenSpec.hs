import Test.Hspec
import SecondTen

main :: IO ()
main = hspec $ do

  describe "second ten of haskell 99 problems" $ do

    it "#11 encodes using custom types" $ do
      encodeModified "aaaabccaadeeee" `shouldBe` do
          [Multiple 4 'a',Single 'b',Multiple 2 'c',
           Multiple 2 'a',Single 'd', Multiple 4 'e']

    it "#11 encodes any Eq typeclass" $ do
      encodeModified [1,1,1,1,2,3,3,1,1,4,5,5,5,5] `shouldBe` do
          [Multiple 4 1,Single 2,Multiple 2 3,
           Multiple 2 1,Single 4, Multiple 4 5]

    it "#12 decodes encoded input 1" $ do
      let encoded = [Multiple 4 'a',Single 'b',Multiple 2 'c',
                     Multiple 2 'a',Single 'd', Multiple 4 'e']
      decodeModified encoded `shouldBe` do "aaaabccaadeeee"

    it "#12 decodes encoded input 2" $ do
      let encoded = [Multiple 4 1,Single 2,Multiple 2 3,
                     Multiple 2 1,Single 4, Multiple 4 5]
      decodeModified encoded `shouldBe` do [1,1,1,1,2,3,3,1,1,4,5,5,5,5]

    it "#14a duplicates a list" $ do
      dupli [1,2,3] `shouldBe` [1,1,2,2,3,3]

    it "#14b duplicates a list" $ do
      dupli "radf" `shouldBe` "rraaddff"

    it "#15a replicates each element in a list n times" $ do
      repli [1,2,3] 3 `shouldBe` [1,1,1,2,2,2,3,3,3]

    it "#15b replicates each element in a list n times" $ do
      repli "trew" 3 `shouldBe` "tttrrreeewww"

    it "#16 drops every nth element" $ do
      (dropEvery "abcdefghik" 3) `shouldBe` "abdeghk"

    it "#17a splits a list" $ do
      splitList [1,2,3,4,5,6,7] 3 `shouldBe` [[1,2,3],[4,5,6,7]]

    it "#17b splits a list" $ do
      splitList "abcdefghik" 3 `shouldBe` ["abc","defghik"]

    it "#17c splits a list" $ do
      splitList [1,2] 3 `shouldBe` [[1,2]]

    it "#17d splits a list" $ do
      splitListTwo [1,2,3,4,5,6,7] 3 `shouldBe` ([1,2,3],[4,5,6,7])

    it "#17e splits a list" $ do
      splitListTwo "abcdefghik" 3 `shouldBe` ("abc","defghik")

    it "#17f splits a list" $ do
      splitListTwo [1,2] 3 `shouldBe` ([1,2], [])

    it "#18a slices a list" $ do
      mySlice "abcdefghijk" 3 7 `shouldBe` "cdefg"

    it "#18b slices a list" $ do
      mySlice "abcd" 3 7 `shouldBe` "cd"

    it "#19a rotates a list" $ do
      myRotate "abcdefgh" 3 `shouldBe` "defghabc"

    it "#19b rotates a list" $ do
      myRotate "abc" 9 `shouldBe` "abc"

    it "#20a remove at n in list" $ do
      myRemoveAt "abcd" 2 `shouldBe` ('b', "acd")

    it "#20b remove at n in list" $ do
      myRemoveAt "abcd" 1 `shouldBe` ('a', "bcd")

    -- it "#20c remove at n in list" $ do
    --   myRemoveAt "abcd" 5 `shouldBe` ('a', "bcd")
