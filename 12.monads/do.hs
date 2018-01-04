-- do notation is just syntatic sugar for
-- chaining >>= (bind) operators

foo :: Maybe Int
foo = return 8
  >>= (\x -> Just (x + 1))
-- or --
foo' :: Maybe Int
foo' = do
  x <- Just 8
  Just (x + 1)

-- we can also chain multiple binds
chain :: Maybe String
chain = return 0
    >>= (\x -> Just (x == 0)
    >>= (\y -> Just (show x ++ show y)
  ))

chain' :: Maybe String
chain' = do
  x <- return 0
  y <- Just (x == 0)
  Just (show x ++ show y)

-- > chain
-- Just "0True"
