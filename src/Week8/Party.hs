module Week8.Party where

import Week8.Employee
import Data.List

-- Exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons e (GL l f) = GL (e : l) (empFun e + f)

instance Semigroup GuestList where 
  (GL l1 f1) <> (GL l2 f2) = let l = union l1 l2  in GL l (sum $ map empFun l)

instance Monoid GuestList where 
  mempty = GL [] 0

  mconcat = foldr mappend mempty 

moreFun :: GuestList -> GuestList -> GuestList
moreFun g1@(GL _ f1) g2@(GL _ f2)
  | f1 >= f2  = g1
  | otherwise = g2

-- Exercise 2

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node a [])     = f a []
treeFold f (Node a forest) = f a (map (treeFold f) forest)
