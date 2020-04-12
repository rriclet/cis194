module Week3.GolfSpec (spec) where

import Test.Hspec
import Week3.Golf

spec :: Spec
spec = do

  describe "Golf.skips" $ do
    it "returns correct lists for 'ABCD'" $ do
      skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"] 
    it "returns correct lists for 'hello!'" $ do
      skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
    it "returns correct lists for [1]" $ do
      skips [1] `shouldBe` [[1]]
    it "returns correct lists for [True,False]" $ do
      skips [True,False] `shouldBe` [[True,False], [False]]
    -- Can't get this test to compile. 
    -- it "returns empty list for an empty list" $ do
      -- skips [] `shouldBe` []

  describe "Golf.nthItems" $ do
    it "returns 3th items for 'ABCDEFG'" $ do
      nthItems 3 "ABCDEFG" `shouldBe` ['C', 'F']
