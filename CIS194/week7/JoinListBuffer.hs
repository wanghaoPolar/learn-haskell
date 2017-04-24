{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module JoinListBuffer where

import           Data.Monoid
import           Data.List

import           Buffer
import           JoinList
import           Scrabble
import           Sized

paragraphToJl :: [String] -> JoinList (Score, Size) String
paragraphToJl [] = Empty
paragraphToJl [str] = Single (scoreString str, Size 1) str
paragraphToJl strList =
  Append
    (tag leftBuffer <> tag rightBuffer)
    leftBuffer
    rightBuffer
  where
    splitPoint = div (length strList) 2
    (left, right) = genericSplitAt splitPoint strList
    leftBuffer = paragraphToJl left
    rightBuffer = paragraphToJl right

instance Buffer (JoinList (Score, Size) String) where
  toString = concat . jlToList
  fromString = paragraphToJl . lines
  line = indexJ
  replaceLine index newStr = replaceJ (index + 1) (scoreString newStr, Size 1) newStr
  numLines (Single _ _) = 1
  numLines (Append (_, Size num) _ _) = num
  value (Single (Score value, _) _) = value
  value (Append (Score value, _) _ _) = value
