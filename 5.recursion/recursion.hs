maximum' :: (Ord a) => [a] -> a
maximum' []     = error "list empty"
maximum' [x]    = x
maximum' (x:xs)
  | x > tailMax = x
  | otherwise   = tailMax
  where tailMax = maximum' xs

replicate' :: (Integral a) => a -> b -> [b]
replicate' 0 _ = []
replicate' times x = [x] ++ replicate' (times-1) x

take' :: (Num a, Ord a) => a -> [b] -> [b]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

contains' :: (Eq a) => a -> [a] -> Bool
contains' _ [] = False
contains' x (y:ys)
  | x == y    = True
  | otherwise = contains' x ys

quickSort' :: (Ord a) => [a] -> [a]
quickSort' [] = []
quickSort' (x:xs) =
  let smallSorted = [a | a <- xs, a <= x]
      largeSorted = [a | a <- xs, a > x ]
  in smallSorted ++ [x] ++ largeSorted
