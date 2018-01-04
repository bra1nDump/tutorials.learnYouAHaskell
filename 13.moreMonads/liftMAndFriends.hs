fmap :: (Functor f) => (a -> b) -> f a -> f b
-- monadic alternative
liftM :: (Monad m) => (a -> b) -> m a -> m b

liftM f m = m >>= (\x -> return (f x))
-- or --
liftM' f m = do
  x <- m
  return (f x)

(<*>) :: (Applicative f) => f (a -> b) -> f a -> f b
-- monadic alternative
ap :: (Monad m) => m (a -> b) -> m a -> m b
ap mf m = mf >>= (\f -> f fmap m)
-- or --
ap' mf m = do
  f <- mf
  x <- m
  return (f x)
