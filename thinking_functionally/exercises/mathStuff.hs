module MathStuff where

type Interval = (Integer, Integer)

floor :: Float -> Integer
floor x = fst $ until found (shrink x) (bound x)

found :: Interval -> Bool
found (m,n) = m+1 == n

leq :: Integer -> Float -> Bool
leq x y = (fromInteger x) <= y

shrink :: Float -> Interval -> Interval
shrink x (m,n) = if p `leq` x then (p,n) else (m,p)
                 where p = choose (m,n)

choose :: Interval -> Integer
choose (x,y) = (x + y) `div` 2

bound :: Float -> Interval
bound x = (lower x, upper x)

upper :: Float -> Integer
upper x = until ((> x) . fromInteger) (*2) 1

lower :: Float -> Integer
lower x = until ((< x) . fromInteger) (*2) (-1)
