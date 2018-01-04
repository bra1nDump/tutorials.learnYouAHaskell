import System.Random


main = do
  generator <- getStdGen
  let (value,_) = randomR (1,100) generator :: (Int,StdGen)
  print value
