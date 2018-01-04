-- import module without select functions --
-- import Data.List hiding (nub)

-- import module to avoid name collisions --
-- import qualified Data.Map as M

import Data.Char
import Data.Function
import Data.List

-- Data.List --
testIntersperce = intersperse '.' "MONKEY"
-- > "M.O.N.K.E.Y"

testIntercalate = intercalate " " ["hey","there","guys"]
-- > "hey there guys"

-- usefull! --
testAny :: Bool
testAny = any (==4) [45,6]

testIterate :: [Int]
testIterate = take 10 . iterate (*2) $ 1

testIterate2 :: [String]
testIterate2 = take 3 $ iterate (++"lolz") ""

testTakeWhile :: [Int]
testTakeWhile = takeWhile (>4) [34,5,66,3,-3]

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
    let nlen = length needle
    in  foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

-- usefull! --
testPartition = partition (>3) [1,3,5,6,3,2,1,0,3,7]
testGroupBy   = groupBy (\x y -> (x > 0) == (y > 0))  [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9]

-- set operators --
-- union, intersect, delete, // (subtract)

-- Caesar chipper --
encode :: Int -> String -> String
encode shift msg =
    let ords = map ord msg
        shifted = map (+ shift) ords
    in  map chr shifted


-- Data.Char --
-- isControl, isSpace, ..
testIsAlphaNum = all isAlphaNum "bobby283"

-- ord - maps a character to its encoding representation
-- use ghci :i to check the precendence of an operator
--                                     2        1
words' xs = filter (not . any isSpace) . groupBy ((==) `on` isSpace) $ xs


-- Data.Map --
find' :: (Eq a) => a -> [(a,b)] -> b
find' _   []         = error "lolx"
find' desiredKey dictionary =
  snd . head . filter (\(key,_) -> key == desiredKey) $ dictionary
