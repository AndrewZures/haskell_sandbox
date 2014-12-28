module FirstTen where

findLast :: [a] -> a
findLast xs = last xs

findSecondToLast :: [a] -> a
-- findSecondToLast xs = last $ init xs
-- findSecondToLast = last . init
findSecondToLast (x:xs)
          | (length xs) == 1  = x
          | otherwise         = findSecondToLast xs

-- ones index
elementAt :: [a] -> Int -> a
-- elementAt xs idx = xs !! (pred idx)
elementAt arr idx = elementAt' arr idx 1

elementAt' :: [a] -> Int -> Int -> a
elementAt' (x:xs) idx inc | idx <= inc   = x
                          | otherwise    = elementAt' xs idx (succ inc)

numElements :: [a] -> Int
-- numElements = length

numElements xs  = numElements' xs 0

numElements' :: [a] -> Int -> Int
numElements'     [] inc = inc
numElements' (x:xs) inc = numElements' xs (succ inc)

myReverse :: [a] -> [a]
-- myReverse = reverse

myReverse [] = []
myReverse (x:[]) = [x]
myReverse (x:xs) = (myReverse xs) ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome []     = True
isPalindrome (x:[]) = True
isPalindrome (x:xs) | x == (last xs) = True && isPalindrome (init xs)
                    | otherwise      = False

myFlatten :: [[a]] -> [a]
myFlatten xss = myFlatten' xss []

myFlatten' :: [[a]] -> [a] -> [a]
myFlatten' [] agg = agg
myFlatten' (xs:xss) agg = myFlatten' xss (agg ++ xs)

myFlatten2 :: [[a]] -> [a]
myFlatten2 xss = foldl (\agg xs -> agg ++ xs) [] xss

myCompress :: (Eq a) => [a] -> [a]
myCompress xs = myCompress' xs []

myCompress' [] agg  = agg
myCompress' (x:xs) agg | x `elem` agg  = myCompress' xs agg
                       | otherwise     = myCompress' xs (agg ++ [x])

myPack :: (Eq a) => [a] -> [[a]]
myPack xs = myPack' xs [] []

myPack' :: (Eq a) => [a] -> [a] -> [[a]] -> [[a]]
myPack' []     tmp agg = tmp : agg
myPack' (x:xs) []  agg = myPack' xs [x] agg
myPack' (x:xs) tmp agg | x == (head tmp)  = myPack' xs (x:tmp) agg
                       | otherwise        = tmp : myPack' xs [x] agg

-- myEncode :: (Eq a) => [a] -> [(Int, a)]
-- myEncode xs = myEncode' xs (0,'a') []
--
-- myEncode' :: (Eq a) => [a] -> (Int, a) -> [(Int, a)] -> [(Int, a)]
-- myEncode' []     tmp agg = tmp : agg
-- myEncode' (x:xs) (0, _)  agg = myEncode' xs (1, x) agg
-- myEncode' (x:xs) tmp agg | x == (snd tmp)  = myEncode' xs (succ fst tmp, snd tmp) agg
--                          | otherwise       = tmp : myEncode' xs (0, 'x') agg
