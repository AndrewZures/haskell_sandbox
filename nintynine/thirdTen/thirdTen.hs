module ThirdTen where

import System.Random

insertAt :: a -> [a] -> Int -> [a]
insertAt _ [] _ = []
insertAt v (x:xs) i | i <= 1    = v:x:xs
                    | otherwise = x : insertAt v xs (pred i)

myRange :: (Ord a, Enum a) => a -> a -> [a]
myRange b t | b < t   = b : myRange (succ b) t
            | b == t  = [b]

-- randomSelect :: [Int] -> Int -> StdGen -> [Int]
-- randomSelect l n gen = take 1 . randomRs (1,6) . gen n

randomSelect :: (RandomGen a) => Int -> Int -> a -> Int -> [Int]
randomSelect x y gen z = take z $ randomRs (x, y) gen

ranIndex :: (RandomGen a) => Int -> a -> Int
ranIndex length gen = head $ randomSelect 0 (pred length) gen 1

-- ranSelect :: (RandomGen a) => [b] -> a -> Int
-- ranSelect xs gen = xs !!
--             where rIdx = ranIndex (length xs) gen

find' :: (Ord a) => a -> [a] -> (a, [a])
find' _ (x:[]) = (x, [])
find' y (x:xs) | x == y    = (x, xs)
               | otherwise = (fst next, x : (snd next))
            where next = find' y xs

takeAt' :: Int -> [a] -> (a, [a])
takeAt' 0 (x:xs)  = (x, xs)
takeAt' _ (x:[])  = (x, [])
takeAt' idx (x:xs) = (fst next, x : (snd next))
                 where next = takeAt' (pred idx) xs

randomSelect' :: (RandomGen b) => Int -> [a] -> b -> ([a],[a])
randomSelect' z xs gen | z <= 0    = ([], xs)
                       | otherwise = (fst taken : fst next, snd next)
              where rIdx = ranIndex (length xs) gen
                    taken = takeAt' rIdx xs
                    next = randomSelect' (pred z) (snd taken) gen

