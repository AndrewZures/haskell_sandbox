module Solver where

import Data.List

smallDivisible :: [Int] -> Maybe Int
smallDivisible divs = find (isDivisibleByAll divs) [jump,start..]
                where jump = startNumber divs
                      start = jump * 2

isDivisibleByAll :: [Int] -> Int -> Bool
isDivisibleByAll divs num = all (\x -> num `mod` x == 0) divs

startNumber :: [Int] -> Int
startNumber = product . smaller

smaller :: [Int] -> [Int]
smaller xs = foldl (\ys z -> map (filterDivisible z) ys) xs xs

filterDivisible :: Int -> Int -> Int
filterDivisible x y = if y `mod` x == 0 && y /= x then 1 else y
