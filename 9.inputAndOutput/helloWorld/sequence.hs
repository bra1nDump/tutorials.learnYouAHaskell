main = do
  results <- sequence [getLine, getLine, getLine]
  print results

-- ghci> sequence (map print [1,2,3,4,5])
-- equivalent to:
-- ghci> mapM print [1,2,3,4,5]
