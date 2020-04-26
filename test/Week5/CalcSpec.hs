module Week5.CalcSpec (spec) where

import Test.Hspec
import Week5.Calc
import Week5.ExprT
import Week5.StackVM
import Week5.Expr

spec :: Spec
spec = do

  describe "Calc.eval" $ do
    it "returns evaluation of expression" $ do
      eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20  

  describe "Calc.evalStr" $ do
    it "returns evaluation of string expression" $ do
      evalStr "(2+3)*4" `shouldBe` Just 20  

  describe "Calc.Expr" $ do
    it "returns equivalent expression in ExprT" $ do
      (mul (add (lit 2) (lit 3)) (lit 4) :: ExprT) `shouldBe` Mul (Add (Lit 2) (Lit 3)) (Lit 4)
    it "returns equivalent expression in Integer" $ do
      (mul (add (lit 2) (lit 3)) (lit 4) :: Integer) `shouldBe` 20
    it "returns equivalent expression in Bool" $ do
      (mul (add (lit 2) (lit 0)) (lit (-1)) :: Bool) `shouldBe` False
    it "returns equivalent expression in MinMax" $ do
      (mul (add (lit 2) (lit 3)) (lit 4) :: MinMax) `shouldBe` MinMax 3
    it "returns equivalent expression in Mod7" $ do
      (mul (add (lit 2) (lit 3)) (lit 4) :: Mod7) `shouldBe` Mod7 6
