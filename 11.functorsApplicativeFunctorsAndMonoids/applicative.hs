-- applicative functors are like functors,
-- but instead of mapping a function over
-- a functor, another functor with a function
-- content type is been mapped over (<*>)
-- another functor

class (Functor f) => Appplicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  (<$>) :: (Functor f) => (a -> b) -> f a -> f b
  f <$> x = fmap f x

instance Applicative Maybe where
  pure x = Just x
  Nothing <*> _ = Nothing
  (Just f) <*> something = fmap f something

-- ghci> Just (+3) <*> Just 9
-- Just 12
-- ghci> pure (+3) <*> Just 10
-- Just 13
-- ghci> pure (+3) <*> Just 9
-- Just 12
-- ghci> Just (++"hahah") <*> Nothing
-- Nothing
-- ghci> Nothing <*> Just "woot"
-- Nothing

instance Applicative [] where
  pure x = [x]
  fs <*> xs = [ f x | f <- fs, x <- xs ]

-- ghci> [(+),(*)] <*> [1,2] <*> [3,4]
-- [4,5,5,6,3,4,6,8]

instance Applicative IO where
  pure = return -- or pure x = return x
  a <*> b = do
    f <- a
    x <- b
    return (f x)

myAction :: IO String
myAction = do
  a <- getLine
  b <- getLine
  return $ a ++ b
-- or --
myAction :: IO String
myAction = (++) <$> getLine <*> getLine


-- applicative functor laws --
-- pure f <*> x = fmap f x
-- pure id <*> v = v
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- pure f <*> pure x = pure (f x)
-- u <*> pure y = pure ($ y) <*> u
