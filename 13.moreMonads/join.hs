join :: (Monad m) => m (m a) -> m a
join mm = mm >>= (\x -> x)
-- or --
join mm = do
  m <- mm
  m

ghci> join (Right (Right 9)) :: Either String Int
-- Right 9
ghci> join (Right (Left "error")) :: Either String Int
-- Left "error"
ghci> join (Left "error") :: Either String Int
-- Left "error"


ghci> runState (join (State $ \s -> (push 10,1:2:s))) [0,0,0]
-- ((),[10,1,2,0,0,0])
