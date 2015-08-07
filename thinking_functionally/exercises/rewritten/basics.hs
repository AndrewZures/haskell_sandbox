module Basics where

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x: myMap f xs

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (xs:xss) = xs ++ myConcat xss

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs) = if f x then x:filterRest else filterRest
                    where filterRest = myFilter f xs

curryFilter :: (a -> Bool) -> [a] -> [a]
curryFilter f = concat . map (test f)

test :: (a -> Bool) -> a -> [a]
test f x = if f x then [x] else []

myZip :: [a] -> [b] -> [(a,b)]
myZip (x:xs) (y:ys) = (x,y):myZip xs ys
myZip  _     _      = []

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith  f (x:xs) (y:ys) = f x y : myZipWith f xs ys
myZipWith  _ _      _      = []

myAnd :: [Bool] -> Bool
myAnd (x:xs) = x && and xs
myAnd []     = True

nonDec :: (Ord a) => [a] -> Bool
nonDec xs = and $ myZipWith (<=) xs (tail xs)

position :: (Eq a) => a -> [a] -> Int
position x ys = position' x ys 0

position' :: (Eq a) => a -> [a] -> Int -> Int
position' x (y:ys) idx | x == y    = idx
                       | ys == []  = -1
                       | otherwise = position' x ys (succ idx)

