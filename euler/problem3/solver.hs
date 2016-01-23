module Solver where
--
-- --  600851475143
--
prime x kps
  | x < 2 = False
  | any (\p -> (rem x p) == 0) kps = False
  | otherwise = True
--
-- intSqrt = floor . sqrt . fromInteger
-- knownPrimes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349]
--
-- startRange x = knownPrimes ++ (initialSieve x)
--
-- initialSieve x = initialSieve' [2..(intSqrt x)] knownPrimes
-- initialSieve' rs [] = rs
-- initialSieve' rs (y:z:ps) = initialSieve' (filter2 y z rs) ps
--
-- primes x = primes' x (startRange x) [2]
--
-- primes' x [] kps = kps
-- primes' x (r:rs) kps
--   | r^2 > x     = kps
--   | prime r kps = primes' x (sieve rs r) (kps ++ [r])
--   | otherwise   = primes' x rs kps
--
-- sieve xs t = filter (\x -> x `rem` t /= 0) xs
--
-- primeFactors x = primeFactors' x (primes x)
--
-- primeFactors' 0 _ = []
-- primeFactors' x (p:ps)
--   | p^2 > x       = [x]
--   | rem x p == 0  = p:(primeFactors' (div x p) (p:ps))
--   | otherwise     = primeFactors' x ps
--
-- largestPrimeFactor x = largestPrimeFactor' x (primes x)
--
-- largestPrimeFactor' x [] = x
-- largestPrimeFactor' x (p:ps)
--   | div x p == 0 = x
--   | rem x p == 0 = largestPrimeFactor' (div x p) (p:ps)
--   | otherwise    = largestPrimeFactor' x ps
--
-- filter2 y z = filter (\x -> x `rem` y /= 0 && x `rem` z /= 0)


newSieve x = newSieve' x 0 [0..x] (length [0..x])

newSieve' x idx rs len
  | idx >= (length rs)   = rs
  | idx < 2              = next
  | rs!!idx == 0         = next
  | otherwise            = newSieve' x (succ idx) nextRange len
  where next = newSieve' x (succ idx) rs len
        nextRange = (take idx rs) ++ (newRange idx (drop idx rs))

newRange idx xs
  | idx > length xs = xs
  | otherwise = (take idx xs) ++ (newRange idx nextRange)
  where nextRange = 0:(drop (idx+1) xs)
