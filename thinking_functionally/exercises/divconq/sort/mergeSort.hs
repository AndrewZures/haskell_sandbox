module MergeSort where

sort :: (Ord a) => [a] -> [a]
sort [] = []
sort [x] = [x]
sort xs = merge (sort ys) (sort zs)
          where (ys, zs) = halve xs

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x < y     = x:merge xs (y:ys)
  | otherwise = y:merge (x:xs) ys

halve :: [a] -> ([a],[a])
halve xs = (take mid xs, drop mid xs)
        where mid = length xs `div` 2
