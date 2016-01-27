module Solver where

isPathTriplet (a,b,c)
  | a > b || b > c   = False
  | a^2 + b^2 == c^2 = True
  | otherwise        = False

tripleFor x = tripleFor' x (1,2,3)
tripleFor' x (a,b,c)
    | isTargetTriplet  = (a,b,newC)
    | a > (x `div` 3)  = (1,1,1)
    | b > (x `div` 2)  = tripleFor' x (succ a, succ a, c)
    | otherwise        = tripleFor' x (a, succ b, c)
    where newC = x - a - b
          isTargetTriplet = a^2 + b^2 == newC^2
