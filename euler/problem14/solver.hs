module Solver where

evenTn n = n `quot` 2
oddTn n = (3 * n) + 1

collatzCnt n = collatzCnt' n 1 True
collatzCnt' n cnt flip
  | n == 1         = cnt
  | n `mod` 2 == 0 = collatzCnt' (evenTn n) (succ cnt) (not flip)
  | otherwise      = collatzCnt' (oddTn n) (succ cnt) (not flip)

maxCollatzCnt x = maxCollatzCnt' 500 x 0
maxCollatzCnt' x ceil best
  | x > ceil     = best
  | otherwise = maxCollatzCnt' (succ x) ceil nextBest
  where thisBest = collatzCnt x
        nextBest = if thisBest > best then thisBest else best
