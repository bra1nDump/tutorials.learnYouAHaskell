import System.Random


randoms' :: (Random a) => StdGen -> [a]
randoms' generator =
  let (value, nextGenerator) = random generator
  in value : randoms' nextGenerator

main = do
  generator <- getStdGen
  let values = take 5 (randoms' generator :: [Bool])
  print values
