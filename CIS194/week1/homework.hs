module Homework where
  toDigitsRev :: Integer -> [Integer]
  toDigitsRev n
    | n <= 0 = []
    | otherwise = mod n 10 : toDigitsRev (n `div` 10)

  toDigits :: Integer -> [Integer]
  toDigits n = reverse' $ toDigitsRev n

  reverse' :: [a] -> [a]
  reverse' [] = []
  reverse' (x:xs) = reverse' xs ++ [x]

  doubleEveryOtherRev :: [Integer] -> [Integer]
  doubleEveryOtherRev [] = []     -- Do nothing to the empty list
  doubleEveryOtherRev [x] = [x]    -- Do nothing to lists with a single element
  doubleEveryOtherRev (x:(y:zs)) = x : (y * 2) : doubleEveryOtherRev zs

  doubleEveryOther :: [Integer] -> [Integer]
  doubleEveryOther ns = reverse' $ doubleEveryOtherRev $ reverse' ns

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
