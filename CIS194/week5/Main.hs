{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Main where

import Lib
import Expr
import ExprT
import ExprParser
import StackVM

instance Expr ExprT where
    lit = Lit
    add = ExprT.Add
    mul = ExprT.Mul

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit n = n > 0
    add a b = a || b
    mul a b = a && b

newtype MinMax  = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
    lit = MinMax
    add (MinMax a) (MinMax b) = MinMax (max a b) 
    mul (MinMax a) (MinMax b) = MinMax (min a b) 

newtype Mod7    = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
    lit n = Mod7 (n `mod` 7)
    add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)
    mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)


eval :: ExprT -> Integer
eval (Lit a) = a
eval (ExprT.Add a b) = eval a + eval b
eval (ExprT.Mul a b) = eval a * eval b

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit ExprT.Add ExprT.Mul 

reify :: ExprT -> ExprT
reify = id

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger  = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

main :: IO ()
main = someFunc
