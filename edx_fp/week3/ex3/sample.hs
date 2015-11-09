module Sample where

and1 :: Bool -> Bool -> Bool
and1 True True = True
and1 _    _    = False

and2 :: Bool -> Bool -> Bool
and2 a b = if a then if b then True else False else False

and3 :: Bool -> Bool -> Bool
and3 a b = if a then b else False

and4 :: Bool -> Bool -> Bool
and4 a b = if b then a else False
