module Solver where

run :: Int -> Int
run x = sum_evens $ dp_find_fibs x

sum_evens :: [Int] -> Int
sum_evens xs = foldl (\ agg x -> if x `mod` 2 == 0 then x + agg else agg) 0 xs

dp_find_fibs :: Int -> [Int]
dp_find_fibs x | x == 0    = [0]
               | x == 1    = [0, 1]
               | otherwise = dp_find' 2 x [0,1]

dp_find' :: Int -> Int -> [Int] -> [Int]
dp_find' i x arr | sum > x = arr
                 | sum == x = _arr
                 | otherwise = dp_find' (succ i) x _arr
                 where
                   sum = arr !! (i-1) + arr !! (i-2)
                   _arr = arr ++ [sum]

