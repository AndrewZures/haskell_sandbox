import Test.Hspec
import Custom

main :: IO ()
main = hspec $ do

  describe "figuring out custom types" $ do

    it "finds surface of circle" $ do
      let point = (Point 0 0)
      surface (Circle point 10) `shouldBe` 314.15927

    it "finds surface of square" $ do
      surface (Rectangle (Point 0 0) (Point 100 100)) `shouldBe` 10000.00

    it "nudges a circle" $ do
      nudge (Circle (Point 0 0) 4) 2 3 `shouldBe` (Circle (Point 2 3) 4)

    it "also nudges a rectangle" $ do
      let rectangle  = Rectangle (Point 0 0) (Point 100 100)
      nudge rectangle 2 3 `shouldBe` (Rectangle (Point 2 3) (Point 102 103))
