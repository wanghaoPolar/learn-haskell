module Homework where

import Data.List

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
     | even x    = (x - 2) * fun1 xs
     | otherwise = fun1 xs

-- point free 下用 . 代替 $
fun1' :: [Integer] -> Integer
fun1' = foldr (\x acc -> (x - 2) * acc ) 1
  . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

-- 注意 filter 和 takewhile 的顺序
fun2' :: Integer -> Integer
-- 如果情况更复杂，就用 foldr (\x acc -> if even n then n * 2 + acc else n + 1 + acc * 2)
fun2' = sum .
  filter even .
  takeWhile (/= 1) .
  iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

data Tree a = Leaf
           | Node Integer (Tree a) a (Tree a)
 deriving (Show, Eq)

treeLevel :: Integer -> Integer
treeLevel n = floor $ logBase 2 (fromInteger n)

-- 用 size 判断应该加到哪一边
size :: Tree a -> Integer
size Leaf = 0
size (Node _ leftTree _ rightTree) = size leftTree + size rightTree + 1

insertNode :: a -> Tree a -> Tree a
insertNode item Leaf = Node 0 Leaf item Leaf
insertNode item (Node level leftTree value rightTree)
  | insertAtLeft = Node newLevel (insertNode item leftTree) value rightTree
  | otherwise = Node newLevel leftTree value (insertNode item rightTree)
  where
    leftSize = size leftTree
    rightSize = size rightTree
    insertAtLeft = leftSize < rightSize
    fullSize = 2 ^ (level + 1) - 1
    newLevel = if leftSize == rightSize && leftSize * 2 + 1 == fullSize then level + 1 else level

foldTree :: [a] -> Tree a
foldTree = foldr insertNode Leaf

xor :: [Bool] -> Bool
-- foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)
xor = foldr (\_ acc -> not acc) False . filter id

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\item acc -> f item : acc) []

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2 * x + 1) $ [1..n] \\ [i + j + 2 * i * j | j <- [1..n], i <- [1..j]]

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)
