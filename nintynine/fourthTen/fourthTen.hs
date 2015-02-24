module FourthTen where

isPrime :: Int -> Bool
isPrime x = isPrime' x 0

isPrime' :: Int -> Int -> Bool
isPrime' x i | _sqrt x >= i  = True
             | x `rem` i == 0 = False
             | otherwise      = isPrime' x (succ i)
             where _sqrt z = floor $ sqrt $ fromIntegral z

-- isPrimelistComp :: Int -> Bool
-- isPrimeListComp x = foldl(\ i s -> s || i*i > v ||) rnTrue

isPrimeS :: Integral a => a -> Bool
isPrimeS k = k > 1 &&
   foldr (\p r -> p*p > k || k `rem` p /= 0 && r)
         True primesTME

primesTME = 2 : gaps 3 (join [[p*p,p*p+2*p..] | p <- primes'])
      where
        primes' = 3 : gaps 5 (join [[p*p,p*p+2*p..] | p <- primes'])
        join  ((x:xs):t)        = x : union xs (join (pairs t))
        pairs ((x:xs):ys:t)     = (x : union xs ys) : pairs t
        gaps k xs@(x:t) | k==x  = gaps (k+2) t
                        | True  = k : gaps (k+2)
