module Week2.LogAnalysisSpec (spec) where

import Test.Hspec
import Week2.Log
import Week2.LogAnalysis

spec :: Spec
spec = do
 
  describe "LogAnalysis.parseMessage" $ do
    it "returns valid error message" $ do
      parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"
    it "return valid info message" $ do 
      parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"
    it "returns valid unknown message" $ do
      parseMessage "This is not in the right format" `shouldBe` Unknown "This is not in the right format"
