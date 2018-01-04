import System.Random

seededRandom :: (Random a) => Int -> a
seededRandom seed =
  let (value, _) = random $ mkStdGen seed
  in value

main = do
  seedString <- getLine
  let seed = (read seedString) :: Int
  putStrLn $ show (seededRandom seed :: Int)
