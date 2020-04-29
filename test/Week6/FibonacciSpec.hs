module Week6.FibonacciSpec (spec) where

import Test.Hspec
import Week6.Fibonacci

spec :: Spec
spec = do

  describe "Fibonacci.fib" $ do
    it "returns fib 8" $ do
      fib 8 `shouldBe` 21  

  describe "Fibonacci.fibs1" $ do
    it "returns first 8 Fibonnacis" $ do
      take 8 fibs1 `shouldBe` [0,1,1,2,3,5,8,13]  

  describe "Fibonacci.fibs2" $ do
    it "returns first 8 Fibonnacis" $ do
      take 8 fibs2 `shouldBe` [0,1,1,2,3,5,8,13]