module Basics where

divides :: Integer -> Integer -> Bool
divides d n = n `rem` d == 0

ld :: Integer -> Integer
ld n = ldf 2 n

ldf :: Integer -> Integer -> Integer
ldf k n | divides k n = k
        | k^2 > n     = n
        | otherwise   = ldf (k+1) n

ldp :: Integer -> Integer
ldp n = ldpf primes n

ldpf :: [Integer] -> Integer -> Integer
ldpf (p:ps) n | divides p n = p
              | p^2 > n     = n
              | otherwise   = ldpf ps n

prime0 :: Integer -> Bool
prime0 n | n <= 1    = False
         | ld n == n = True
         | otherwise = False

prime1 :: Integer -> Bool
prime1 n = n > 1 && ld n == n

primes :: [Integer]
primes = filter prime1 [2..]

removeFst :: Integer -> [Integer] -> [Integer]
removeFst n [] = []
removeFst n (x:xs) | x == n    = xs
                   | otherwise = x : removeFst n xs

sort :: (Ord a) => [a] -> [a]
sort [] = []
sort [x] = [x]
sort xs = merge (sort left) (sort right)
          where (left, right) = halve xs

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x > y     = y:merge (x:xs) ys
    | otherwise = x:merge xs (y:ys)

halve :: [a] -> ([a],[a])
halve xs = (take mid xs, drop mid xs)
        where mid = length xs `div` 2

otherSort [] = []
otherSort xs = mint:(otherSort $ removeFst mint xs)
              where mint = minInt xs

minInt [x] = x
minInt (x:y:xs) | x > y = minInt (y:xs)
                | otherwise = minInt (x:xs)

length' :: [a] -> Integer
length' [] = 0
length' (x:xs) = 1 + length' xs

factors :: Integer -> [Integer]
factors n | n < 1     = error "error"
          | n == 1    = []
          | otherwise = p:(factors $ div n p)
          where p = ld n

lengths :: [[a]] -> [Integer]
lengths xss = map length' xss

sumLengths :: [[a]] -> Integer
sumLengths xss = sum $ lengths xss
