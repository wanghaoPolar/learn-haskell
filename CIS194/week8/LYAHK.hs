module LYAHK where

import System.Environment
import System.Directory
import System.Random
import System.IO
import Data.List
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S

-- byteString
copyFile' :: FilePath -> FilePath -> IO ()
copyFile' source dest = do  
    contents <- B.readFile source
    B.writeFile dest contents

-- random
randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value:randoms' newGen

finiteRandoms :: (RandomGen g, Random a, Num n, Eq n) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
    let (value, newGen) = random gen
        (restOfList, finalGen) = finiteRandoms (n-1) newGen
    in  (value:restOfList, finalGen)

getRandom :: [String] -> IO ()
getRandom [_] = do
  gen <- getStdGen
  putStr $ take 20 (randomRs ('a', 'z') gen)

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let number = read numberString
        todoTasks = lines contents
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName

dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("add", add)
             , ("vie<!-- w", view)
             , ("remov -->e", remove)
            ]

main :: IO ()
main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args
