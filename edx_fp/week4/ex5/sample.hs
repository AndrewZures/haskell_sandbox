module Sample where
import Data.Char

find :: (Eq a) => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: (Eq a) => a -> [a] -> [Int]
positions k xs = find k (zip xs [0..(length xs - 1)])

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- (zip xs ys)]

let2int :: Char -> Char -> Int
let2int c x = ord c - ord x

int2let :: Int -> Char -> Char
int2let n x = chr (ord x + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c 'a' + n) `mod` 26) 'a'
  | isUpper c = int2let ((let2int c 'A' + n) `mod` 26) 'A'
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

divides :: Int -> Int -> Bool
divides x y = x `mod` y == 0

divisors :: Int -> [Int]
divisors x = [d | d <- [1..x], divides x d]
