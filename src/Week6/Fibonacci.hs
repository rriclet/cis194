{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Week6.Fibonacci where

-- Exercise 1

fib :: Int -> Int 
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Int]
fibs1 = map fib [0..]

-- Exercise 2

fibs2 :: [Int]
-- not my solution, didn't think / know of zipWith at the time 
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList   

streamToList :: Stream a -> [a]
streamToList (Cons x s) = x : streamToList s

-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x s) = Cons (f x) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamMap f $ streamFromSeed f x)

-- Exercise 5 

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap (powersOfTwo . (+1)) nats) 

-- not my function, I tried to find a answer without divisibily testing. No sucess ! 
powersOfTwo :: Integer -> Integer
powersOfTwo n 
  | odd n  = 0
  | even n = 1 + powersOfTwo (n `div` 2)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x s1) (Cons _ (Cons y s2)) = Cons x (Cons y (interleaveStreams s1 s2))

-- Exercise 6

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
   fromInteger n = Cons n (streamRepeat 0)

   negate (Cons x xs) = Cons x (streamMap negate xs)

   (+) (Cons x xs) (Cons y ys) = Cons (x + y) (xs + ys)

   (*) (Cons x xs) s2@(Cons y ys) = Cons (x * y) (streamMap (*x) ys + (xs * s2))

instance Fractional (Stream Integer) where
  (/) s1@(Cons x xs) s2@(Cons y ys) = Cons (x `div` y) (streamMap (`div` y) $ xs - (s1 / s2) * ys)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2) 
 