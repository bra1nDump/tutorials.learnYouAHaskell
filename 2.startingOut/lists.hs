doubleUs x y = (x * 2, y * 2)

doubleSmallNumber x = if x > 10
                        then x
                        else x * 2

doubleSmallNumber' x = (if x > 10 then x else x * 2) + 1

fuckThisList list = head $ reverse list

sayHelloWorld :: IO ()
sayHelloWorld = putStrLn $ "hello" ++ " " ++ "world"

-- I should find a way to make this shit
-- in s less cluttered way!
-- try logging all the activity to
-- one string and only than printing
-- it out
test :: IO ()
test = do
  putStrLn "lool"
  putStrLn ('l' : "another loo")
  putChar ("pidrila" !! 2)
  putStrLn ""
  putStrLn $ "[4,2] > [4,1]  - " ++ (if [4,2] > [4,1] then "true" else "false")
  putStrLn $ "[4,50] > [5,0] - " ++ (if [4,50] > [5,0] then "true" else "false")

-- operations on list
testList :: IO ()
testList = do
  putStrLn log where
    log = ""
      ++ (show $ head list)
      ++ (show $ tail list)
      ++ (show $ last [4,5,6])
      ++ (show $ init [4,5,6])
      ++ "\n"
      ++ (show $ take 2 list)
      ++ (show $ maximum list + product list)
      ++ "\n"
      ++ (show $ elem 5 list)
      ++ (show $ elem 6 list) where
        list = [1,2,3,4,5]

testList' = do
  putStrLn $ take 50 (cycle "pidrila lol ")
  putStrLn $ take 50 (repeat '5')
  putStrLn $ replicate 50 '5'

boomBags xs = [ if x < 10 then "boom" else "bang" | x <- xs ]
