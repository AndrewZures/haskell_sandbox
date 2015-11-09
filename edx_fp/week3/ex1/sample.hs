module Sample where

safeTail1 :: [a] -> [a]
safeTail1 xs = if null xs then [] else tail xs

safeTail2 :: [a] -> [a]
safeTail2 [] = []
safeTail2 (_:xs) = xs

safeTail3 :: [a] -> [a]
safeTail3 xs
  | null xs   = []
  | otherwise = tail xs

safeTail4 :: [a] -> [a]
safeTail4 [] = []
safeTail4 xs = tail xs

safeTail5 :: [a] -> [a]
safeTail5 = \xs -> case xs of
                     [] -> []
                     (_:xs) -> xs

