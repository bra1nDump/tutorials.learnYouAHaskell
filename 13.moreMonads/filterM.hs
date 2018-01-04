import Control.Applicative

liftA2' :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2' f x y = f <$> x <*> y

filterM' :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM' p xs = foldr (
  \x acc -> -- -> newAcc (type: m [a])
    (p x) >>= (
      \flag ->
        if flag
        then do
          accContents <- acc
          return (x:accContents)
        else
          acc
      )
  ) (return []) xs

filterM'' :: (Applicative m) => (a -> m Bool) -> [a] -> m [a]
filterM'' p = foldr (
  \x -> -- acc -> newAcc :: m [a]
    liftA2 (\flag -> -- :: a -> (b -> c)
        if flag
        then (x:) -- :: b -> c (where b <- acc, or list from the accumulator monad)
        else (id) -- :: b -> c (in our case b == c = <- m a)
      ) (p x) -- (p x) is the first monad, from wich flag <- m Bool is extracted
      -- this whole lift thing returns a function of newtyp
      -- :: m a -> m a
  ) (pure [])

-- filterM (\x -> [True, False]) [45, 4, 1]

-- > filterM (\x -> if x > 3 then Just True else Nothing) [45, 4, 6]
-- Just [45,4,6]
-- > filterM (\x -> if x > 3 then Just True else Nothing) [45, 4, 6, 2]
-- Nothing

keepSmall :: Int -> Writer [String] Bool
keepSmall x
    | x < 4 = do
        tell ["Keeping " ++ show x]
        return True
    | otherwise = do
        tell [show x ++ " is too large, throwing it away"]
        return False

-- ghci> mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [9,1,5,2,10,3]
-- 9 is too large, throwing it away
-- Keeping 1
-- 5 is too large, throwing it away
-- Keeping 2
-- 10 is too large, throwing it away
-- Keeping 3

powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs
