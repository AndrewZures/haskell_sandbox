module Sample where

-- something is wrong with my funtion signature
remove1 :: Int -> [a] -> [a]
remove1 n xs = (take n xs) ++ (drop (n + 1) xs)

mystery :: Int -> [a] -> [a]
mystery n xs = take (n + 1) xs ++ drop n xs
