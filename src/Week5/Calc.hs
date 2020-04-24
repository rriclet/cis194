{-# LANGUAGE FlexibleInstances #-}

module Week5.Calc 
( eval
, evalStr
, MinMax(..)
, Mod7(..)
, compile
) where

import Week5.ExprT
import Week5.Parser
import Week5.StackVM
import Week5.Expr
import Data.Maybe

eval :: ExprT -> Integer
eval e = case e of
             (Lit n)     -> n
             (Add e1 e2) -> eval e1 + eval e2
             (Mul e1 e2) -> eval e1 * eval e2

evalStr :: String -> Maybe Integer
evalStr s = fmap eval (parseExp Lit Add Mul s)

instance Expr ExprT where
  lit = Lit
  add = Add 
  mul = Mul

instance Expr Integer where
  lit x = x
  add x y = x + y 
  mul x y = x * y

instance Expr Bool where
  lit x   = x > 0
  add x y = x || y
  mul x y = x && y

newtype MinMax = MinMax Integer 
  deriving (Eq, Show)

instance Expr MinMax where
  lit                       = MinMax
  add (MinMax x) (MinMax y) = MinMax $ max x y 
  mul (MinMax x) (MinMax y) = MinMax $ min x y

newtype Mod7 = Mod7 Integer
  deriving (Eq, Show)

instance Expr Mod7 where 
  lit x                 = Mod7 $ x `mod` 7
  add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7 
  mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7

instance Expr Program where 
  lit x   = [PushI x]
  add x y = x ++ y ++ [Add'] 
  mul x y = x ++ y ++ [Mul']

compile :: String -> Maybe Program
compile = parseExp lit add mul
