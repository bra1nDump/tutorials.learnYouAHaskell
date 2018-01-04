import System.Environment (getArgs)
import System.Directory (removeFile, renameFile)
import System.IO (readFile, openFile, openTempFile, hPutStr)
import Data.List

-- api --
-- view   fileName
-- add    fileName item
-- remove fileName indexString

dispatch :: [(String,[String] -> IO ())]
dispatch = [
    ("view",   view),
    ("add",    add),
    ("remove", remove)
  ]


view :: [String] -> IO ()
view [fileName] = do
  todoList <- readFile fileName
  let items = lines todoList
      numberedItems = zipWith (
          \index item ->
          show index ++ " - " ++ item
        ) [0..] items
  putStrLn $ unlines numberedItems

add :: [String] -> IO ()
add [fileName, item] = do
  appendFile fileName (item ++ "\n")

remove :: [String] -> IO ()
remove [fileName,indexString] = do
  (temporaryFileName,temporaryHandle) <- openTempFile "." "temp"
  todoList <- readFile fileName
  let items = lines todoList
      index = read indexString
      newItems = delete (items !! index) items
      newTodoList = unlines newItems
  hPutStr temporaryHandle newTodoList
  removeFile fileName
  renameFile temporaryFileName fileName

main = do
  (command:arguments) <- getArgs
  let (Just action) = lookup command dispatch
  action arguments
