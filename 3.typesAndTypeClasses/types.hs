removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

testParametrization = do
  show $ addThree 4 5 1
  show $ (addThree 4 5) 1

factorial :: Int -> Int
factorial n = product [1..n]

factorial' :: Integer -> Integer
factorial' n = product [1..n]

test = do
  putStrLn $ show $ "Abrakadabra" < "Zebra"
  putStrLn $ show $ "Abrakadabra" `compare` "Zebra"
  putStrLn $ show $ (read "True") || False
  putStrLn $ show $ (read "4.5") + 5.0
