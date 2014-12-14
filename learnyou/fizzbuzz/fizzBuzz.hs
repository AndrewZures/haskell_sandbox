module FizzBuzz where

run :: [Int] -> [String]
run xs = [ match x | x <- xs ]

match :: Int -> String
match n | n `mod` 15 == 0  = "fizzbuzz"
            | n `mod` 5  == 0  = "buzz"
            | n `mod` 3  == 0  = "fizz"
            | otherwise        = show n
