-- The context for functions is that that value
-- is not present yet and that we have to apply
-- that function to something in order to get its result value

-- instance Monad ((->) r) where
--   return x = \_ -> x -- or pure
--
--   -- m a >>= (a -> m b) -> m b
--   -- (->) a >>= (a -> ((->) b)) -> ((->) b)
--   h >>= f = (\x -> f (h x) x)

addStuff :: Int -> Int
addStuff = (*2)
  >>= (\a -> (+3)
  >>= (\b -> (a + b)))
-- or --
addStuff' :: Int -> Int
addStuff' = do
  a <- (*2)
  b <- (+3)
  return (a + b)
-- or --
addStuff'' :: Int -> Int
addStuff'' x = let
  a = (*2) x
  b = (+10) x
  in a+b
