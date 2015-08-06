module RightTriangle where

find :: Int -> [(Int, Int, Int)]
find n = [(x,y,z) | x <- [1..n], y <- [x+1..n], coprime x y,
                    z <- [y+1..n], x*x + y*y == z*z]

coprime :: Int -> Int -> Bool
coprime x y = disjoint (divisors x) (divisors y)

divisors :: Int -> [Int]
divisors n = [x | x <- [2..n-1], n `mod` x == 0]

disjoint :: [Int] -> [Int] -> Bool
disjoint [] _ = True
disjoint (x:xs) ys | x `elem` ys = False
                   | otherwise   = disjoint xs ys

lcMap :: (a -> b) -> [a] -> [b]
lcMap f xs = [f x | x <- xs]

lcFilter :: (a -> Bool) -> [a] -> [a]
lcFilter f xs = [x | x <- xs, f x]

lcConcat :: [[a]] -> [a]
lcConcat xss = [x | xs <- xss, x <- xs]
