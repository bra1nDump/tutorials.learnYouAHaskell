-- `A monoid` `is when`? you have an associative
-- binary function and a value which acts
-- as an identity with respect to that function
-- comment: grammatically correct

-- discrete mathematics --
-- monoid - a group with one binary
-- function that has an identity
-- element and is associative

-- exmples --
-- (real numbers, *), where identity element - 1
-- (lists, ++), where identity element - empty list = []

class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty

-- where mempty :: m stands for identity element
-- or a `polymorphic constant`
-- kind of like `minBound` from Bounded
-- mappend - binary function

-- monoid laws --
-- * mempty `mappend` x = x
-- * x `mappend` mempty = x
-- * (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

instance Monoid [a] where
  mempty = []
  mappend = (++)
  -- or mappend xs ys = xs ++ ys

newtype All = All { getAny :: Bool }
  deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid (All a) where
  mempty = True
  (All x) `mappend` (All y) = (x && y)

instance (Monoid a) => Monoid (Maybe a) where
  mempty = Nothing
  Nothing `mappend` (Just x) = Just x
  (Just x) `mappend` Nothing = Just x
  (Just x) `mappend` (Just y) = Just (x `mappend` y)

-- ghci> Just (Sum 3) `mappend` Just (Sum 4)
-- Just (Sum {getSum = 7})

-- Foldable + Monoid --
foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m

instance Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Node x l r) =
    foldMap f l `mappend`
    f x         `mappend`
    foldMap f r

testTree = Node 5
  (Node 3
    (Node 1 Empty Empty)
    (Node 6 Empty Empty)
  )
  (Node 9
    (Node 8 Empty Empty)
    (Node 10 Empty Empty)
  )

-- ghci> foldl (+) 0 testTree
-- 42
-- ghci> foldl (*) 1 testTree
-- 64800
