import qualified Data.List as L
import qualified Data.Map.Lazy as M
import           System.Directory (removeFile, renameFile)
import           System.Environment
import           System.IO
import           Text.Printf (printf)

funcMap = M.fromList [ ("show", show')
                     , ("add",  add')
                     , ("done", done')
                     ]

-- List the current todo list.
show' :: String -> IO ()
show' file =
  withFile file
           ReadMode
           (\h -> do
               contents <- hGetContents h
               putStr . L.unlines . L.zipWith (\num line -> show num ++ ". " ++ line) [0..] . L.lines $ contents
           )

-- Add a new action to the todo list.
add' :: String -> IO ()
add' file = do
  putStrLn "Please specify task:"
  task <- getLine
  appendFile file $ task ++ "\n"

-- Mark a current action as done.
done' :: String -> IO ()
done' file = do
  putStrLn "Please specify task number:"
  taskNum    <- getLine
  (tempPath, tempH) <- openTempFile "/tmp/" ".haskell_notebook"
  let num = read taskNum
  withFile file
           ReadWriteMode
           (\h -> do
               contents <- hGetContents h
               hPutStr tempH . L.unlines . (\lines -> L.delete (lines !! num) lines) . L.lines $ contents
           )
  hClose tempH
  removeFile file
  renameFile tempPath file

main :: IO ()
main = do
  progName   <- getProgName
  homeDir    <- getEnv "HOME"
  (action:_) <- getArgs
  if M.member action funcMap
  then
    funcMap M.! action $ homeDir ++ "/.haskell_notebook"
  else do
    printf "Usage: %s show|add|done\n" progName
    return ()
