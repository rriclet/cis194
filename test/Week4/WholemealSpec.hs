module Week4.WholemealSpec (spec) where

import Test.Hspec
import Week4.Wholemeal

spec :: Spec
spec = do

  describe "Wholemeal.fun1'" $ do
    it "returns same output as fun1" $ do
      fun1' [4,5,2] `shouldBe` fun1 [4,5,2]  

  describe "Wholemeal.fun2'" $ do
    it "returns same output as fun2" $ do
      fun2' 8 `shouldBe` fun2 8
    it "returns same output as fun2" $ do
      fun2' 7 `shouldBe` fun2 7
      
  describe "Wholemeal.foldTree" $ do
    it "returns a balanced tree" $ do
      foldTree "ABCDEF" `shouldBe` Leaf  
