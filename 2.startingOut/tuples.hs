test = do
  putStrLn $ show $ fst (1,4)
  putStrLn $ show $ snd (1,4)
  putStrLn $ show $ zip [1,2,3,4,5] [5,5,5,5,5]
  -- haskell - lazy, thus uses the shortest
  -- list to zip
  putStrLn $ show $ zip [1,2,3,4,5] [5,5,5]
  putStrLn $ show $ zip [1,3,4] (cycle [55])
