{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Main where

import Control.Applicative
import qualified Data.Map as M
import Expr
import ExprT
import ExprParser
import StackVM

instance Expr ExprT where
    lit = ExprT.Lit
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
eval (ExprT.Lit a) = a
eval (ExprT.Add a b) = eval a + eval b
eval (ExprT.Mul a b) = eval a * eval b

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp ExprT.Lit ExprT.Add ExprT.Mul

reify :: ExprT -> ExprT
reify = id

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger  = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

compile :: String -> Maybe Program
compile = parseExp lit add mul

-- Simply create an instance of the Expr type class for Program,
-- so that arithmetic expressions can be interpreted as compiled programs.
instance Expr Program where
  lit n = [PushI n]
  add x y = x ++ y ++ [StackVM.Add]
  mul x y = x ++ y ++ [StackVM.Mul]

class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String Integer
    deriving (Show, Eq)

instance Expr VarExprT where
  lit = Main.Lit
  add = Main.Add
  mul = Main.Mul

main :: IO ()
main = print (stackVM Control.Applicative.<$> compile "(3 * -4) + 5")
