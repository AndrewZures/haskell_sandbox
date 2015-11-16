import Test.Hspec
import Sample

main :: IO ()
main = hspec $ do

  describe "something" $ do

    describe "option 1" $ do

      it "implements basics find" $ do
        find 1 [(1, 'a'), (1, 'b')] `shouldBe` ['a', 'b']

      it "finds the positions of an item given the find function" $ do
        positions 'a' "abac" `shouldBe` [0,2]

      it "finds the scalar product" $ do
        scalarproduct [1,2,3] [4,5,6] `shouldBe` 32

      it "caesar cipher with uppercases" $ do
        encode 13 "Think like a Fundamentalist Code like a Hacker" `shouldBe` "Guvax yvxr n Shaqnzragnyvfg Pbqr yvxr n Unpxre"

      it "shows divide 15 2" $ do
        divides 15 2 `shouldBe` False

      it "shows divide 15 3" $ do
        divides 15 3 `shouldBe` True

      it "divisors" $ do
        divisors 15 `shouldBe` [1,3,5,15]

