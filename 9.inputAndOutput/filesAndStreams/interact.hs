import Data.Char

interact' :: (String -> String) -> IO ()
interact' f = do
  contents <- getContents
  putStrLn $ f contents

main = interact' someTransformationOfInput

someTransformationOfInput :: String -> String
someTransformationOfInput input =
  map toUpper input
