isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

ghci> isBigGang 3
-- (False,"Compared gang size to 9.")
ghci> isBigGang 30
-- (True,"Compared gang size to 9.")

applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f =
  let (y, nextLog) = f x
  in (y, log ++ nextLog)

ghci> (3, "Smallish gang.") `applyLog` isBigGang
-- (False,"Smallish gang.Compared gang size to 9")
ghci> (30, "A freaking platoon.") `applyLog` isBigGang
-- (True,"A freaking platoon.Compared gang size to 9")

-- now a more generic version for lists --
applyLog :: (a, [c]) -> (a -> (b, [c])) -> (b, [c])

-- what if we would want to use it with bytestrings?
-- we would have to change the types, but instead of that
-- we can use monoids `mappend` function

-- now using monoids --
applyLog (Monoid m) :: (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f =
  let (y, nextLog) = f x
  in (y, log `mappend` nextLog)

-- example with newtype Sum monoid
import Data.Monoid

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food,Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

ghci> Sum 3 `mappend` Sum 9
-- Sum {getSum = 12}

ghci> ("beans", Sum 10) `applyLog` addDrink
-- ("milk",Sum {getSum = 35})
ghci> ("jerky", Sum 25) `applyLog` addDrink
-- ("whiskey",Sum {getSum = 124})
ghci> ("dogmeat", Sum 5) `applyLog` addDrink
-- ("beer",Sum {getSum = 35}) 
