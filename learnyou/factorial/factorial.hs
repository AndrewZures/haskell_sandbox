module Factorial where

run :: Int -> Int
run 0 = 1
run x = x * run(x-1)
