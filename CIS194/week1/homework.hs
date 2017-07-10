module Homework where

import Data.List

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = mod n 10 : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n = reverse $ toDigitsRev n

doubleEveryOtherFromLeft :: [Integer] -> [Integer]
doubleEveryOtherFromLeft [] = []     -- Do nothing to the empty list
doubleEveryOtherFromLeft [x] = [x]    -- Do nothing to lists with a single element
doubleEveryOtherFromLeft (x:(y:zs)) = x : (y * 2) : doubleEveryOtherFromLeft zs

isEven :: Int -> Bool
isEven n = mod n 2 == 0

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (n:ns) =
  if isEven $ length ns
    then doubleEveryOtherFromLeft $ n:ns
    else n * 2 : doubleEveryOtherFromLeft ns

sumDigits :: [Integer] -> Integer
sumDigits ns = sum $ map (sum . toDigits) ns

validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther $ toDigits n) `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 a b c = []
hanoi n a b c =
  hanoi (n - 1) a c b ++
  [(a, b)] ++
  hanoi (n - 1) c b a
