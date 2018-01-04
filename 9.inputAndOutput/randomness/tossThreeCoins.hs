import System.Random


tossThreeCoins :: StdGen -> (Bool,Bool,Bool)
tossThreeCoins generator =
  let (coin,generator')   = random generator   :: (Bool,StdGen)
      (coin',generator'') = random generator'  :: (Bool,StdGen)
      (coin'',_)          = random generator'' :: (Bool,StdGen)
  in (coin,coin',coin'')

-- getStdGen alwais returns the same stdGen instance
-- for all the conseqent calls within one programm,
-- thus (coins) == (coins')
main = do
  generator <- getStdGen
  print $ tossThreeCoins generator
  generator' <- getStdGen
  print $ tossThreeCoins generator'
