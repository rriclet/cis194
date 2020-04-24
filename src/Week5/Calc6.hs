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
{-
I looked up the answer online but I can't understand the syntax.
Maybe I need more knowledge of Haskell. For example, the answer for add is :
  add x y m = liftM2 (+) (x m) (y m)
Where is this m coming from ?

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x    = M.lookup "a"  
  add x y = x + y
  mul x y = x * y
-}
withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
