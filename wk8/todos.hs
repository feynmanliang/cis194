import System.Environment
import System.IO
import System.Directory

todosFile :: FilePath
todosFile = "todos.txt"

dispatch :: [(String, [String] -> IO ())]
dispatch = [ ("add", add)
           , ("remove", remove)
           , ("view", view)
           ]

main :: IO ()
main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args


add :: [String] -> IO ()
add args = withFile todosFile AppendMode (\handle ->
  hPutStrLn handle (init . unlines $ args))

remove :: [String] -> IO ()
remove args = withFile todosFile ReadWriteMode (\handle -> do
  contents <- hGetContents handle
  (tmpFilePath, tmpFileHandle) <- openTempFile "." "temp"
  let itemNum = read (head args) :: Int
      fileLines = lines contents
      newContents = take itemNum fileLines ++ drop (itemNum+1) fileLines
  hPutStr tmpFileHandle $ unlines newContents
  hClose tmpFileHandle
  removeFile todosFile
  renameFile tmpFilePath todosFile)


view :: [String] -> IO ()
view _ = do
  contents <- readFile todosFile
  let numberedLines = zipWith (\n line -> show n ++ "-" ++ line) [(0::Int)..] (lines contents)
  putStr (unlines numberedLines)
