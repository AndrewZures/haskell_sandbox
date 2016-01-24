module Solver where

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x =  x == reverse x

toArray :: (Integral a) => a -> [a]
toArray = reverse. toArray'

toArray' :: (Integral a) => a -> [a]
toArray' 0 = []
toArray' num = num `mod` 10 : toArray' (num `div` 10)

highestPalindrome range = maximum $ filter (isPalindrome . toArray) $ genOptions range

genOptions range = [x*y | x <- range, y <- range]
