module Main where

import Week1.Intro
import Week2.Log
import Week2.LogAnalysis

main :: IO [LogMessage]
main = do
  testParse parse 10 "src/Week2/error.log"
