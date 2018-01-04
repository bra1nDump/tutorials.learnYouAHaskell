applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing _ -> Nothing
applyMaybe (Just x) f = f x

-- applyMaybe == (>>=)

-- examples --
-- ghci> Just 3 `applyMaybe` \x -> Just (x+1)
-- Just 4
-- ghci> Just "smile" `applyMaybe` \x -> Just (x ++ " :)")
-- Just "smile :)"
-- ghci> Nothing `applyMaybe` \x -> Just (x+1)
-- Nothing
-- ghci> Nothing `applyMaybe` \x -> Just (x ++ " :)")
-- Nothing

instance Monad (Maybe a) where
  return = Just

  Nothing >>= f = Nothing
  (Just x) >>= f = f x
