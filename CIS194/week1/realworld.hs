module Baby where
{-
  chapter 1 get start
  use :info to check the precedence level of each operator
  :info (+)
    class (Eq a, Show a) => Num a where
    (+) :: a -> a -> a
    ...
    	-- Defined in GHC.Num
  infixl 6 +

  Preceden Level 1 + 4 * 4 = 1 + (4 * 4)
  level 高的先计算
  infixl 6 +
  infixl 7 *

  Associativity: 连续使用时的方向 x ^ y ^ z
  infixr 8 ^ (for integer)
  infixr 8 ** (for float)

  ':' adds an element to the front of a list
  1 : [2,3]

  use :set +t to show type
  use :unset +t to hide it
-}

main = interact wordCount
    where wordCount input = show (length [x | x <- input, x `notElem` [' ', '\n', ',']]) ++ "\n"
    -- where charCount input = show (length (concat (words input))) ++ "\n"
{-
  chapter 2 type
  - strong
    no coerce(不会隐式转换)
  - static
    执行之前 compiler 就知道每个值的类型

  use Double instead of Float

  Functions apply left association
  a b c d = ((a b) c) d
-}

-- || operate short circuits
myDrop :: Int -> [a] -> [a]
myDrop n xs = if n < 0 || null xs
  then xs
  else myDrop (n - 1) (tail xs)

-- lazy
-- The result of applying a function may be a thunk (a deferred expression).
isOdd n = mod n 2 == 1

lastButOne :: [a] -> Maybe a
lastButOne [x] = Nothing
lastButOne [x,y] = Just x
lastButOne (x:xs) = lastButOne xs

hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3*n + 1

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo [] = []     -- Do nothing to the empty list
sumEveryTwo [x] = [x]    -- Do nothing to lists with a single element
sumEveryTwo (x:(y:zs)) = (x + y) : sumEveryTwo zs

{-
  For now, the take-home message is:
  don’t be afraid to write small functions
  that transform whole data structures,
  and combine them to produce more complex functions.
-}
hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

intListLength :: [Integer] -> Integer
intListLength [] = 0
intListLength (_:xs) = 1 + intListLength xs

hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n) - 1
