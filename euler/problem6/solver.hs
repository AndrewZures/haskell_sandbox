module Solver where

sumOfSquares xs = sum $ [x^2 | x <- xs]

squareOfSums xs = (^2) $ sum xs

diff xs = (squareOfSums xs) - (sumOfSquares xs)
