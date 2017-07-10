{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Main where

import Data.List

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = map fst $ iterate (\(x, y) -> (y, x + y)) (0, 1)

data Stream a = S a (Stream a)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (S a b) = a:streamToList b

streamRepeat :: a -> Stream a
streamRepeat n = S n $ streamRepeat n

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (S a b) = S (f a) (streamMap f b)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = S a (streamFromSeed f $ f a)

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (S a b) (S x y) = S a $ S x $ interleaveStreams b y

x :: Stream Integer
x = S 0 $ S 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger n = S n $ streamRepeat 0
  negate = streamMap (\a -> a * (-1))
  (+) (S a1 a) (S b1 b) = S (a1 + b1) (a + b)
  (*) (S a1 a) (S b1 b) = S (a1 * b1) $ streamMap (\bx -> a1 * bx) b + a * S b1 b

div :: Stream Integer -> Stream Integer -> Stream Integer
div (S a1 a) (S b1 b) = S (Prelude.div a1 b1) $ streamMap (`Prelude.div` b1) (a - (Main.div (S a1 a) (S b1 b) * b))

fibs3 :: Stream Integer
fibs3 = Main.div x (1 - x - x ^ 2)

data Matrix = Matrix Integer Integer Integer Integer deriving (Show)

instance Num Matrix where
  (*) (Matrix a1 a2 a3 a4) (Matrix b1 b2 b3 b4) =
    Matrix (a1*b1 + a2*b3) (a1*b2 + a2*b4) (a3*b1 + a4*b3) (a3*b2 + a4*b4)

fib4 :: Integer -> Integer
fib4 n = a1 where
  Matrix a1 _ _ _ = Matrix 1 1 1 0 ^ n

main :: IO ()
main = print "main"
