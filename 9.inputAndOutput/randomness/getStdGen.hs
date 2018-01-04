import System.Random

-- value == value' /= value''
main = do
  generator <- getStdGen
  let (value, _) = random generator :: (Char,StdGen)
      (value', generator') = random generator :: (Char,StdGen)
      (value'',_) = random generator' :: (Char,StdGen)
  putStrLn $ show value ++ show value' ++ show value''
