-- lists are another example of monads --
-- p.s. lists also act as monoids
-----------------------------------------

instance Monad [] where
  return = pure -- return x = [x]
  xs >>= f = concat (fmap f xs)

foo :: [String]
foo = [5, -8]
    >>= (\x -> [3, 6]
    >>= (\y -> [(x, y)]
  ))
-- or --
foo' :: [String]
foo' = do
  x <- [5, -8]
  y <- [3, 6]
  return (x, y)
-- or --
foo'' :: [String]
foo'' = [(x, y) |
    x <- [5, -8],
    y <- [3, 6]
  ]

-- guard statements --
----------------------
-- (1) *
ghci> [ x | x <- [1..50], '7' `elem` show x ]
-- [7,17,27,37,47]

class Monad m => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

instance MonadPlus [] where
  mzero = []
  mplus = (++)

guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero

ghci> guard (5 > 2) :: Maybe ()
-- Just ()
ghci> guard (1 > 2) :: Maybe ()
-- Nothing
ghci> guard (5 > 2) :: [()]
-- [()]
ghci> guard (1 > 2) :: [()]
-- []

-- (1) **
ghci> [1..50]
  >>= (\x -> guard ('7' `elem` show x)
  >> return x)
-- [7,17,27,37,47]

-- (2)
ghci> guard (5 > 2) >> return "cool" :: [String]
-- ["cool"]
ghci> guard (1 > 2) >> return "cool" :: [String]
-- []

-- guard + do
sevensOnly :: [Int]
sevensOnly = do
  x <- [1..50]
  guard ('7' `elem` show x)
  return x
