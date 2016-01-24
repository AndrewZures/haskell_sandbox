module Solver where

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x =  x == reverse x

toArray :: (Integral a) => a -> [a]
toArray = reverse. toArray'

toArray' :: (Integral a) => a -> [a]
toArray' 0 = []
toArray' num = num `mod` 10 : toArray' (num `div` 10)

highestPalindrome :: Int
highestPalindrome = max . map palindromValue
