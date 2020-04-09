module Main where

import Intro

main :: IO ()
main = do
  putStrLn "toDigits 1234 == [1,2,3,4]"
  print $ toDigits 1234 == [1,2,3,4]
  putStrLn "toDigitsRev 1234 == [4,3,2,1]"
  print $ toDigitsRev 1234 == [4,3,2,1]
  putStrLn "toDigits 0 == []"
  print $ toDigits 0 == []
  putStrLn "toDigits (-17) == []"
  print $ toDigits (-17) == []

