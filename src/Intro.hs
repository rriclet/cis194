module Intro ( toDigits, 
               toDigitsRev,
               doubleEveryOther,
               sumDigits,
               validate,
               hanoi
) where

-- Credit card validation

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0    = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse $ toDigits n

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n = reverse $ doubleEveryOtherUtil $ reverse n

doubleEveryOtherUtil :: [Integer] -> [Integer]
doubleEveryOtherUtil [] = []
doubleEveryOtherUtil [x] = [x]
doubleEveryOtherUtil (x:xs) = x : (head xs * 2) : [] ++ doubleEveryOtherUtil (tail xs)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) 
  | x < 10    = x + sumDigits xs
  | otherwise = sumDigits $ toDigits x ++ xs

validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther $ toDigits x) `mod` 10 == 0

-- Tower of Hanoi

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _         = []
hanoi n from to other = hanoi (n-1) from other to ++ [(from, to)] ++ hanoi (n-1) other to from
