module Solver where

run :: [Int] -> Int
run xs = foldl (\acc x -> acc + eval(x)) 0 $ init xs

eval :: Int -> Int
eval x = if x `mod` 3 == 0 || x `mod` 5 == 0 then x
         else 0
