import System.Environment
import System.IO
import Control.Exception
import System.IO.Error

main = toTry `catch` handler

toTry :: IO ()
toTry = do
  (fileName:_) <- getArgs
  contents <- readFile fileName
  putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"


-- readFile exception types --
-- isAlreadyExistsError
-- isDoesNotExistError
-- isAlreadyInUseError
-- isFullError
-- isEOFError
-- isIllegalOperation
-- isPermissionError
-- isUserError
------------------------------

handler :: IOError -> IO ()
handler e
  | isDoesNotExistError e =
    case ioeGetFileName e of
      Just path -> putStrLn $ "Whoops! File does not exist at: " ++ path
      Nothing   -> putStrLn "Whoops! File does not exist at unknown location!"
  | otherwise             = ioError e
