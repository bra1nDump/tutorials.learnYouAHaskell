import Data.Char

-- getContents takes the input from the
-- standart input
-- cat haiku.txt | ./getContents
main = do
  input <- getContents
  putStrLn $ shortLinesOnly input

shortLinesOnly :: String -> String
shortLinesOnly input =
  let allLines = lines input
      shortLines = filter (\line -> length line < 30) allLines
      output = unlines shortLines
  in output
