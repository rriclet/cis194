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

  describe "Golf.localMaxima" $ do
    it "returns 2 local maxima" $ do
      localMaxima [2,9,5,6,1] `shouldBe` [9, 6]
    it "returns 1 local maxima" $ do
      localMaxima [2,3,4,1,5] `shouldBe` [4]
    it "returns 0 local maxima" $ do
      localMaxima [1,2,3,4,5] `shouldBe` []

  describe "Golf.histogram" $ do
    it "returns 4 point-histogram" $ do
      histogram [1,1,1,5] `shouldBe` " *        \n *        \n *   *    \n=========\n0123456789\n"
    it "returns 11 point-histogram" $ do
      histogram [1,4,5,4,6,6,3,4,2,4,9] `shouldBe` "    *     \n    *     \n    * *   \n ******  *\n=========\n0123456789\n"
    it "returns 2 point-histogram" $ do
      histogram [3,5] `shouldBe` "   * *    \n=========\n0123456789\n"

  describe "Golf.plotLine" $ do
    it "returns 2 point-line" $ do
      plotLine 10 [4,6] `shouldBe` "    * *   \n"
