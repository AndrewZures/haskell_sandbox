module PrimeGenerator where

import Data.Array

-- genPrimes :: Int -> [Int]
-- genPrimes = sieve . genBaseArray


-- genPrimes x =  (idx, array) (2, genBaseArray x)

primes = filter (/=0) . elems . snd . sieveAll . wrap . genBaseArray


wrap arr = (11, arr)

-- extractPrimes = filter (/=0) gcc

sieveAll (idx, arr)
    | fst sieved > arrLength arr = (idx, arr)
    | otherwise = sieveAll sieved
    where sieved = sieve (idx, arr)

arrLength = snd . bounds

sieve (idx, array)
    | (array ! idx) /= 0 = (succ idx, array // genRemList idx max)
    | otherwise          = (succ idx, array)
    where max = arrLength array

genRemList val max = [(x,0) | x <- [jump,start..max]]
                  where jump = val*2
                        start = val*3

genBaseArray ceil = array(2, ceil) [(i,removeEvens i) | i <- [2..ceil]]

removeEvens x
  | x `mod` 2 == 0 && x /= 2 = 0
  | x `mod` 3 == 0 && x /= 3 = 0
  | x `mod` 5 == 0 && x /= 5 = 0
  | x `mod` 7 == 0 && x /= 7 = 0
  | otherwise = x
