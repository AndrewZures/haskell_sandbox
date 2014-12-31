module SecondTen where

data Encoded a = Single a | Multiple Int a deriving (Show, Eq)

-- #11
encodeModified :: (Eq a) => [a] -> [Encoded a]
encodeModified [] = []
encodeModified (x:xs) = encodeModified' xs (Single x)


encodeModified' :: (Eq a) => [a] -> Encoded a -> [Encoded a]
encodeModified' [] enc = [enc]

encodeModified' (x:xs) (Single v)
                | x == v    = encodeModified' xs (Multiple 2 v)
                | otherwise = (Single v) : encodeModified' xs (Single x)

encodeModified' (x:xs) (Multiple i v)
                | x == v    = encodeModified' xs (Multiple (succ i) v)
                | otherwise = (Multiple i v) : encodeModified' xs (Single x)


-- #12
decodeModified :: (Eq a) => [Encoded a] -> [a]
decodeModified [] = []
-- decodeModified (x:xs) = (decode' x) : decodeModified xs
decodeModified (x:xs) = (decode' x) ++ decodeModified xs

decode' :: (Eq a) => Encoded a -> [a]
decode' (Single v) = [v]
decode' (Multiple i v) = replicate i v

-- #skipping #13 for now

-- #14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : (dupli xs)

-- #15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = (replin x n) ++ (repli xs n)

replin :: a -> Int -> [a]
replin x n  = replin' x n 0

replin' :: a -> Int -> Int -> [a]
replin' x n inc | n <= inc   = []
                | otherwise  = x : replin' x n (succ inc)

-- #16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n  = dropEvery' xs n 1

dropEvery' :: [a] -> Int -> Int -> [a]
dropEvery' [] _ _ = []
dropEvery' (x:xs) n i | (i `mod` n) == 0   = next
                      | otherwise          = x : next
                      where next = dropEvery' xs n (succ i)
