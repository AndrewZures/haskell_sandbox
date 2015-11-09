module Game where

type Row a = [a]
type Matrix a = [Row a]
type Grid = Matrix Digit
type Digit = char

digits :: [Digit]
digits = ['1'..'9']

blank :: Digit -> Bool
blank = (== '0')

solve = filter valid . completions

completions :: Grid -> [Grid]
valid :: Grid -> Bool
