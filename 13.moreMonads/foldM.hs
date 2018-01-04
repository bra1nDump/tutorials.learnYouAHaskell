foldM' :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM' f initial = foldl (
  \accumulator x -> do
    content <- accumulator
    f content x
  ) (return initial)

binSmalls :: Int -> Int -> Maybe Int
binSmalls acc x
  | x > 9     = Nothing
  | otherwise = Just (acc + x)


-- ghci> foldM binSmalls 0 [2,8,3,1]
-- Just 14
-- ghci> foldM binSmalls 0 [2,11,3,1]
-- Nothing
