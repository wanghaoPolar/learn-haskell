{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrabble where

import           Data.Char

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

score :: Char -> Score
score t =
    if | upT `elem` "AEILNORSTU" -> Score 1
       | upT `elem` "DG" -> Score 2
       | upT `elem` "BCMP" -> Score 3
       | upT `elem` "FHVWY" -> Score 4
       | upT `elem` "K" -> Score 5
       | upT `elem` "JX" -> Score 8
       | upT `elem` "QZ" -> Score 10
       | otherwise -> Score 0
       where
        upT = toUpper t

scoreString :: String -> Score
scoreString = foldr (\x (Score acc) -> let (Score nx) = score x in Score $ acc + nx) (Score 0)
