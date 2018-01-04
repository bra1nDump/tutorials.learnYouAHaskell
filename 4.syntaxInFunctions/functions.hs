lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- this pattern matching can fail!
-- does not contain a catch all
-- statement in the end & previous
-- cases are not exaustive
charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- tuple getters
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

test = do
  putStrLn log where
    log = show [a+b | (a,b) <- xs] where
    xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]

head' :: [a] -> a
head' [] = error "cannot get the head of an empty list"
head' (x:xs) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- pattern matching with reference
-- to the initial argument
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- guards --

-- bmiTell :: (RealFloat a) => a -> String
-- bmiTell bmi
--   | bmi <= 18.5 = "You're underweight, you emo, you!"
--   | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
--   | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
--   | otherwise   = "You're a whale, congratulations!"

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "You're underweight, you emo, you!"
  | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= fat    = "You're fat! Lose some weight, fatty!"
  | otherwise     = "You're a whale, congratulations!"
  where bmi = weight / height ^ 2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)

bmiCalc :: (RealFloat a) => [(a,a)] -> [a]
bmiCalc xs = [ bmi weight height | (weight, height) <- xs ]
  where bmi weight height = weight / height ^ 2

max' :: (Ord a) => a -> a -> a
max' a b
  | a > b     = a
  | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a > b     = GT
  | a == b    = EQ
  | otherwise = LT

cylinderArea :: (RealFloat a) => a -> a -> a
cylinderArea r h =
  let baseArea = pi * r ^ 2
      sideArea = h * 2 * pi * r
  in 2 * baseArea + sideArea

headCase :: [a] -> a
headCase xs = case xs of []    -> error "empty list"
                         (x:_) -> x

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                              [x] -> "a singleton list."
                                              xs -> "a longer list."

describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs  
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."
