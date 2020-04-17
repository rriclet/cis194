module Week4.Wholemeal ( fun1
                       , fun1'
                       , fun2
                       , fun2'
                       , Tree(Leaf, Node)
                       , foldTree
) where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
    | even n    = n + fun2 (n `div` 2)
    | otherwise = fun2 (3*n + 1)

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2' :: Integer -> Integer
-- Didn't found the correct answer
fun2' x = x

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertTree Leaf

insertTree :: a -> Tree a -> Tree a
insertTree x t = case t of
    Leaf                                 -> Node 0 Leaf x Leaf
    (Node height left@Leaf a right@Leaf) -> Node (height+1) (insertTree x left) a right
    (Node height left@Leaf a right)      -> Node height (insertTree x left) a right 
    (Node height left a right@Leaf)      -> Node height left a (insertTree x right)
    (Node height l@(Node lHeight _ _ _) a r@(Node rHeight _ _ _)) -> 
                                       if lHeight <= rHeight 
                                       then Node (lHeight+1) (insertTree x l) a r
                                       else Node (rHeight+1) l a (insertTree x r)
