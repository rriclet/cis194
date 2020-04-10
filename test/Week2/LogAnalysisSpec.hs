module Week2.LogAnalysisSpec (spec) where

import Test.Hspec
import Week2.Log
import Week2.LogAnalysis

spec :: Spec
spec = do
 
  describe "LogAnalysis.parseMessage" $ do
    it "returns " $ do
      parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"
