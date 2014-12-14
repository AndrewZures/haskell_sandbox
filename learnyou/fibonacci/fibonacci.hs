module Fibonacci where

-- recursive fibonacci approach
find :: Int -> Int
find x | x == 0    = 0
       | x == 1    = 1
       | otherwise = find(x - 1) + find(x - 2)

-- dynamic programming fibonacci approach (much faster)
dp_find :: Int -> Int
dp_find x | x == 0    = 0
          | x == 1    = 1
          | otherwise = dp_find' 2 x [0,1]

dp_find' :: Int -> Int -> [Int] -> Int
dp_find' i x arr | i == x    = sum
                 | otherwise = dp_find' (succ i) x (arr ++ [sum])
                 where
                   sum = arr !! (i-1) + arr !! (i-2)

