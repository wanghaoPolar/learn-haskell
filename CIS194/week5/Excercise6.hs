{-# LANGUAGE FlexibleInstances #-}

import qualified Data.Map as M

class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String
    deriving (Show, Eq)

instance Expr VarExprT where
  lit = Main.Lit
  add = Main.Add
  mul = Main.Mul

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit n = Main.Lit
