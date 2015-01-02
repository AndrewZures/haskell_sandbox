module ThirdTen where

insertAt :: a -> [a] -> Int -> [a]
insertAt _ [] _ = []
insertAt v (x:xs) i | i <= 1    = v:x:xs
                    | otherwise = x : insertAt v xs (pred i)

myRange :: (Ord a) => (Enum a) => a -> a -> [a]
myRange b t | b < t   = b : myRange (succ b) t
            | b == t  = [b]

randomSelect :: [a] -> Int -> [a]
