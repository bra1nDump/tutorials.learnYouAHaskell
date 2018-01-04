-- return wrapes a value type in an
-- IO action

main = do
  a <- return "hell"
  b <- return "yeah!"
  putStrLn $ a ++ " " ++ b
