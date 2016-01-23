isBigGange :: Int -> (Bool, String)
isBigGange x = (x > 9, "Compared gang size of 9")


-- applyLog :: (a,String) -> (a -> (b,String)) -> (b,String)
-- applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)

-- applyLog :: (a, [c]) -> (a -> (b, [c]) -> (b, [c])
--
applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
applyLog (x,log) f = let (y, newLog) = f x in (y, log `mappend` newLog)

import Data.Monoid

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)
