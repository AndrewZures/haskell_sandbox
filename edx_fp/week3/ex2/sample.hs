module Sample where

or1 :: Bool -> Bool -> Bool
or1 False False = False
or1 _     _     = True

or2 :: Bool -> Bool -> Bool
or2 False b = b
or2 True _  = True

or3 :: Bool -> Bool -> Bool
or3 b c
  | b == c = b
  | otherwise = True

or4 :: Bool -> Bool -> Bool
or4 b False = b
or4 _ True = True

or5 :: Bool -> Bool -> Bool
or5 False False = False
or5 False True = True
or5 True False = True
or5 True True = True
