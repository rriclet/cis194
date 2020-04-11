module Main where

import Week1.Intro
import Week2.Log
import Week2.LogAnalysis

main :: IO [String]
main = do
  -- testParse parse 10 "src/Week2/error.log"
  testWhatWentWrong parse whatWentWrong "src/Week2/error.log"
  -- testBuild parse build "src/Week2/sample.log"
