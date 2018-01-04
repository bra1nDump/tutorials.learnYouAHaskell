-- stateful computation type
s -> (a, s)

-- example
random :: StdGen -> (a, StdGen)

-- stack --
type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push x xs = ((), x:xs)

manipulateStack :: Stack -> (Int, Stack)
manipulateStack stack = let
  (a, stack') = pop stack
  (_, stack'') = push (44 + a) stack'
  in pop stack''

-- desired behavior
manipulateStack' :: State Stack Int
manipulateStack' = pop
  >>= (\poppedValue -> push (44 + poppedValue)
  >>= (\_ -> pop))
-- or --
manipulateStack'' :: State Stack Int
manipulateStack'' = do
  a <- pop
  push (44 + a)
  pop

-- state monad --
newtype State s a = {
    runState :: s -> (a, s)
  }

instance Monad (State s) where
  return x = (\s -> (x, s))
  -- (>>=) :: m a -> (a -> m b) -> m b
  -- (>>=) :: State s a -> (a -> State s b) -> State s b
  (State h) >>= f = State $
    \s ->
      let (a, newState) = h s
          (State g) = f a
      in g newState

pop' :: State Stack Int
pop' = State \(x:xs) -> (x, xs)

push' :: Int -> State Stack Int
push' x = State \xs -> ((), (x:xs))
