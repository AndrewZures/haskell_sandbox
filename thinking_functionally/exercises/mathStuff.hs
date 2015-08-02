module MathStuff where

type Interval = (Integer, Integer)

-- floor :: Float -> Integer
-- floor x = until found (shrink x) (bound x)
--           where found m n = m+1 == n
--
-- need a < that can evaluate floats to integer
--
shrink :: Float -> Interval -> Interval
shrink x (m,n) = if ((fromInteger p) <= x) then (p,n) else (m,p)
                 where p = choose (m,n)

choose :: Interval -> Integer
choose (x,y) = (x + y) `div` 2

bound :: Float -> Interval
bound x = (lower x, upper x)

upper :: Float -> Integer
upper x = until ((> x) . fromInteger) (*2) 1

lower :: Float -> Integer
lower x = until ((< x) . fromInteger) (*2) (-1)
