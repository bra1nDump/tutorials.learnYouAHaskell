class (Applicative m) => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b

-- Monads are a natural extension of applicative
-- functors and with them we're concerned with this:
-- if you have a value with a context, m a,
-- how do you apply to it a function that takes
-- a normal a and returns a value with a context?
--
-- That is, how do you apply a function of
-- type a -> m b to a value of type m a
