-- we use newtype when we want to
-- create a new type from an excisting
-- type
-- newtype constructor can only have
-- one field constructor

-- example
newtype ZipList a = ZipList {
    getZipList :: [a]
  }
newtype CharList = CharList {
    getCharList :: [Char]
  } deriving (Eq, Show)

-- Using newtype to make type class instances
newtype Pair b a = Pair {
    getPair :: (a,b)
  }

-- pattern matching also works,
-- as you can see the matchingg occures
-- using the underliying type
instance Functor (Pair c) where
  fmap f (Pair (x,y)) = Pair (f x, y)

data CoolBool = CoolBool {
    getCoolBool :: Bool
  }

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

-- ghci> helloMe undefined
-- "*** Exception: Prelude.undefined

newtype CoolBool = CoolBool {
    getCoolBool :: Bool
  }

-- ghci> helloMe undefined
-- "hello"

-- explanation --
-- And because Haskell knows that types made with
-- the newtype keyword can only have one constructor,
-- it doesn't have to evaluate the value passed to
-- the function to make sure that it conforms to
-- the (CoolBool _) pattern because newtype types
-- can only have one possible value constructor and one field
--
