newtype Writer w a = {
    runWriter :: (a, w)
  }

instance (Monoid w) => Monad (Writer w) where
  return x = Writer (x, mempty)
  (Writer (x, v)) >>= f =
    let (y, v') = f x
    in Writer (y, w `mappend` w')

ghci> getWriter (return 8 :: Writer String Int)
-- (8, "")
ghci> runWriter (return 3 :: Writer (Sum Int) Int)
-- (3,Sum {getSum = 0})
ghci> runWriter (return 3 :: Writer (Product Int) Int)
-- (3,Product {getProduct = 1})

-- writer + do notation
import Control.Monad.Writer

logNumber :: Int -> Writer [String] Int
logNumber x = Writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  return (a*b)

ghci> runWriter multWithLog
-- (15,["Got number: 3","Got number: 5"])

multWithLog' :: Writer [String] Int
multWithLog' = do
  a <- logNumber 3
  b <- logNumber 5
  tell ["Gonna multiply these two"]
  return (a*b)

ghci> runWriter multWithLog  
-- (15,["Got number: 3","Got number: 5","Gonna multiply these two"])
