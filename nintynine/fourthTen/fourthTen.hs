module FourthTen where

basic :: (Num a) => a -> a -> a
basic x y = x - y          -- for basic Euclidean Algorithm

optimized :: (Integral a, Num a) => a -> a -> a
optimized a b = a `mod` b  -- for optimized Euclidean Algorithm


-- Greatest Common Divisor using Euclidean Algorithm
myGcd :: (Num a, Ord a) => a -> a -> (a -> a -> a) -> a
myGcd x 0 _ = x
myGcd 0 y _ = y
myGcd x y alg
  | x < y     = myGcd x (y-x) alg
  | y < x     = myGcd (x-y) y alg
  | otherwise = x

