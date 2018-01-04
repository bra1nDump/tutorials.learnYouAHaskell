-- functor laws --
-- * fmap (f . g) = fmap f . fmap g
--   or * fmap (f . g) F = fmap f (fmap g F)
--     where F is a functor
-- * fmap (id . f) = id . fmap f


-- f is a type parameter, which
class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor IO where
  fmap f action = do
    result <- action
    return (f result)

instance Functor Maybe where
  fmap f Nothing = Nothing
  fmap f (Just x) = Just f x

-- functions are members of the
-- Functor class, thus they can be
-- mapped over. for functions
-- fmap is just function composition
instance Functor ((->) r) where
  fmap f g = (\x -> f (g x))
-- or --
instance Functor ((->) r) where
  fmap = (.)
