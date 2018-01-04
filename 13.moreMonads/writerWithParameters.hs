import Control.Monad.Writer

gcd' :: Int -> Int -> Int
gcd' a b
  | b == 0 = a
  | otherwise = gcd' b (a `mod` b)

-- now lets equip our function with logging
-- capabilities
gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
  | b == 0 = do
    tell ["Finished with " ++ show a]
    return a
  | otherwise = do
    tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
    return gcd' b (a `mod` b)

ghci> fst $ runWriter (gcd' 8 3)
-- 1

ghci> mapM_ putStrLn $ snd $ runWriter (gcd' 8 3)
-- 8 mod 3 = 2
-- 3 mod 2 = 1
-- 2 mod 1 = 0
-- Finished with 1

-- log is produced like this:
-- a ++ (b ++ (c ++ (d ++ (e ++ f))))
-- more efficient
gcdLogReverse :: Int -> Int -> Writer [String] Int
gcdLogReverse a b
  | b == 0 = do
    tell ["Finished with " ++ show a]
    return a
  | otherwise = do
    result <- gcdReverse b (a `mod` b)
    tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
    return result
