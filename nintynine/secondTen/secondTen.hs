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
decodeModified (x:xs) = (decode' x) ++ decodeModified xs

decode' :: (Eq a) => Encoded a -> [a]
decode' (Single v) = [v]
decode' (Multiple i v) = replicate i v
