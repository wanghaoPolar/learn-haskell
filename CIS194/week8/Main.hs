module Main where

import Control.Monad
import System.IO
-- Remember, to get the value out of an I/O action,
-- you have to perform it inside another I/O action by binding it to a name with <-.

main :: IO [()]
main =  do
    colors <- forM [1,2,3,4] (\a -> do
       putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
       getLine)
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
    mapM putStrLn colors

interactMain :: IO ()
interactMain = interact respondPalindromes

respondPalindromes = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") . lines
    where   isPalindrome xs = xs == reverse xs

fileIO :: IO ()
fileIO = do
    handle <- openFile "girlfriend.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
    handle <- openFile path mode
    result <- f handle
    hClose handle
    return result

    --withFile "girlfriend.txt" ReadMode (\handle -> do
    --    contents <- hGetContents handle
    --    putStr contents)

readFileIO :: IO ()
readFileIO = do
    contents <- readFile "girlfriend.txt"
    putStr contents
--main = do
    -- return is the opposite of <-
    -- a <- return "hell"
    -- b <- return "yeah!"
--    let
--      a = "hell"
--      b = "yeah!"
--    putStrLn $ a ++ " " ++ b
--    line <- getLine
--    unless (null line) $
--      do putStrLn $ reverseWords line
--         main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
