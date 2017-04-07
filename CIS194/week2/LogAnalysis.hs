{-# LANGUAGE MultiWayIf #-}
module HomeWork where

import Data.Char
import Control.Applicative

data MessageType = Info
                 | Warning
                 | Error Int
  deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
  deriving (Show, Eq)

data MessageTree = Leaf
                 | Node MessageTree LogMessage MessageTree
  deriving (Show, Eq)

parseMessage :: String -> LogMessage
parseMessage messageString =
    case words messageString of
        "E":int:timestamp:info -> LogMessage (Error $ read int) (read timestamp) (unwords info)
        "I":timestamp:info     -> LogMessage Info (read timestamp) (unwords info)
        "W":timestamp:info     -> LogMessage Warning (read timestamp) (unwords info)
        info                   -> Unknown (unwords info)

parse :: String -> [LogMessage]
parse fileString =
    map parseMessage $ lines fileString

testParse :: (String -> [LogMessage])
          -> Int
          -> FilePath
          -> IO [LogMessage]
testParse parse n file = take n . parse <$> readFile file

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert log Leaf = Node Leaf log Leaf
insert log (Node leftTree nodeLog rightTree) =
    if | ts1 > ts2 -> Node leftTree nodeLog (insert log rightTree)
       | ts1 < ts2 -> Node (insert log leftTree) nodeLog rightTree
    where
        (LogMessage _ ts1 _) = log
        (LogMessage _ ts2 _) = nodeLog

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree log rightTree) =
  inOrder leftTree ++
  [log] ++
  inOrder rightTree

whatWentWrong :: [LogMessage] -> (LogMessage -> Bool) -> [String]
whatWentWrong xs include = recur $ inOrder $ build $ filter include xs
  where
    recur [] = []
    recur (x:xs) = let (LogMessage _ _ info) = x
                 in info:recur xs

testLog :: (String -> [LogMessage])
                  -> ([LogMessage] -> [String])
                  -> FilePath
                  -> IO [String]
testLog parse checker file
  = checker . parse <$> readFile file
