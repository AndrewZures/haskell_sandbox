module Game where

type Row a = [a]
type Matrix a = [Row a]
type Digit = char
type Grid = Matrix Digit

digits :: [Char]
digits = ['1'..'9']

blank :: Digit -> Bool
blank = (== '0')

solve :: Grid -> [Grid]
solve = filter valid . completions

completions :: Grid -> [Grid]

valid :: Grid -> Bool

-- option 1
completions = expand . choices

choices :: Grid -> Matrix [Digits]
choices = map (map choice)

choice :: Digit -> [Digit]
choice d = if blank d then digits else [d]

cp :: [[a]] -> [[a]]
cp [] => [[]]
cp (xs:xss) = [(x:ys) | x <- xs, ys <- yss]
            where yss <- cp xss

-- cp: cartesian product via recursion
-- [[2], [1,3]] -> [[2,1], [2,3]]
-- [2] -> [[1],[3]]
-- [[2,1], [2,3]]

expand :: Matrix [Digits] -> [Grid]
expand = cp . map cp

valid g = all nodups (rows g) &&
          all nodups (cols g) &&
          all nodups (boxs g)

nodups :: [Digit] -> Bool
nodups [] = True
nodups (x:xs) = all (/= x) xs && nodups xs

rows :: Matrix a -> Matrix a
rows = id

-- cols :: Matrix a -> Matrix a
-- cols [xs] =

