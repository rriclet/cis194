module Week5.CalcSpec (spec) where

import Test.Hspec
import Week5.Calc
import Week5.ExprT

spec :: Spec
spec = do

  describe "Calc.eval" $ do
    it "returns evaluation of expression" $ do
      eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20  
