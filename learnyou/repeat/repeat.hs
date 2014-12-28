module Repeat where

run :: (Num i, Ord i) => i -> a -> [a]
run n x
  | n <= 0    = []
  | otherwise = x:run (n-1) x

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x

zipWith' :: (a -> b -> c) -> [a] -> [b] ->[c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip f = g
    where g x y = f y x

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
      | f x        = x : filter' f xs
      | otherwise  = filter' f xs


