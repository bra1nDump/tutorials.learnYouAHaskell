-- continiously read lines and print them
-- with reversed order until blanck line is read

main :: IO ()
main = do
  line <- getLine
  if null line
    then return ()
    else do
      putStrLn $ reverseWordOrder line
      putStrLn $ reverseEachWord  line
      main

reverseWordOrder = unwords . reverse . words
reverseEachWord  = unwords . map reverse . words
