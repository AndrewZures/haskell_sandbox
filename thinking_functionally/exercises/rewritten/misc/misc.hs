module Misc where

ascDisjoint :: (Ord a) => [a] -> [a] -> Bool
ascDisjoint [] _ = True
ascDisjoint _ [] = True
ascDisjoint xs'@(x:xs) ys'@(y:ys)
    | x > y  = ascDisjoint xs' ys
    | x < y  = ascDisjoint xs ys'
    | x == y = False
