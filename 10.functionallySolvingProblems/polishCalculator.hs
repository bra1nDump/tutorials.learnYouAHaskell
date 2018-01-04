import System.Environment

main = do
  (expression:_) <- getArgs
  let answer = evaluatePolishNotationExpression expression
  print answer

evaluatePolishNotationExpression :: (Num a, Read a) => String -> a
evaluatePolishNotationExpression =
  head . foldl updateStack [] . words
  where
    updateStack (x:y:ys) "+" = (x + y):ys
    updateStack (x:y:ys) "-" = (x - y):ys
    updateStack (x:y:ys) "*" = (x * y):ys
    updateStack ys numberString = read numberString:ys

-- alternative definition --
-- evaluatePolishNotationExpression expression =
--   let tokens = words expression
--   in head $ foldl updateStack [] tokens
