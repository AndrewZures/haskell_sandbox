module PrimeGenerator where

import Data.Array

-- genPrimes :: Int -> [Int]
-- genPrimes = sieve . genBaseArray


-- genPrimes x =  (idx, array) (2, genBaseArray x)

sieve (idx, array)
    | (array ! idx) /= 0 = (succ idx, array // (genRemList idx (snd $ bounds array)))
    | otherwise          = (succ idx, array)

genRemList val max = [(x,0) | x <- [jump,start..max]]
                  where jump = val*2
                        start = val*3

genBaseArray ceil = array(1, ceil) [(i,i) | i <- [1..ceil]]
