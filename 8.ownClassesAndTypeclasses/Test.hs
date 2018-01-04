module Test (cumulativeTest) where

import BinaryTree

cumulativeTest :: IO ()
cumulativeTest = putStrLn log where
  log = show sampleTree
  sampleTree = foldr insert EmptyTree numbers
  numbers = [3,4,5,7,8,2,6,4] :: [Int]
