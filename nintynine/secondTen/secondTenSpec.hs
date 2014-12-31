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




