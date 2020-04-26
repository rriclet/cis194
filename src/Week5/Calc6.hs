{-# LANGUAGE FlexibleInstances #-}

module Week5.Calc6 
( VarExprT(..)
, var
, withVars
) where

import Week5.Expr
import qualified Data.Map as M

class HasVars a where
  var :: String -> a 

data VarExprT = Lit Integer
              | Var String
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = Lit
  add = Add 
  mul = Mul

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x m   = Just x  
  add x y m = (+) <$> x m <*> y m
  mul x y m = (*) <$> x m <*> y m

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
