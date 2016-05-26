module Solver where

factorCount x = factorCount' 2 x 0
factorCount' idx target count
  | idx > floored = count
  | calc == 0     = factorCount' (succ idx) divved (count+2)
  | otherwise     = factorCount' (succ idx) target count
  where calc = target `mod` idx
        divved = target `div` idx
        floored = (floor $ sqrt $ fromInteger target)

hey x = hey' 3 3 0 x
hey' x idx maxFnd targCnt
  | thisCount >= targCnt = x
  | thisCount > maxFnd   = hey' nextCount (succ idx) thisCount targCnt
  | otherwise            = hey' nextCount (succ idx) maxFnd    targCnt
  where thisCount = factorCount x
        nextCount = x + idx
