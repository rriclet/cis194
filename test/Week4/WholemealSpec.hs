module Week4.WholemealSpec (spec) where

import Test.Hspec
import Week4.Wholemeal

spec :: Spec
spec = do

  describe "Wholemeal.fun1'" $ do
    it "returns same output as fun1" $ do
      fun1' [4,5,2] `shouldBe` fun1 [4,5,2]  
{-|
  describe "Wholemeal.fun2'" $ do
    it "returns same output as fun2" $ do
      fun2' 8 `shouldBe` fun2 8
    it "returns same output as fun2" $ do
      fun2' 7 `shouldBe` fun2 7
-}      
  describe "Wholemeal.foldTree" $ do
    it "returns a balanced tree" $ do
      foldTree "ABCDEF" `shouldBe` Node 2 (Node 1 (Node 0 Leaf 'C' Leaf) 'E' (Node 0 Leaf 'A' Leaf)) 'F' (Node 1 (Node 0 Leaf 'B' Leaf) 'D' Leaf)  

  describe "Wholemeal.xor'" $ do
    it "returns True if odd number of True's" $ do
      xor [False, True, False] `shouldBe` True
    it "returns True if even number of True's" $ do
      xor [False, True, False, False, True] `shouldBe` False

  describe "Wholemeal.map''" $ do
    it "returns same as map" $ do
      map (3*) [1,2,3,4] `shouldBe` [3,6,9,12]
    it "returns same as map" $ do
      map' reverse ["abc","cda","1234"] `shouldBe` ["cba","adc","4321"] 
{-|
  describe "Wholemeal.myFoldl" $ do
    it "returns same as foldr" $ do
      myFoldl (-) 0 [1..10] `shouldBe` (-5)
-}
