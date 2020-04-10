import Test.Hspec
import Intro

main :: IO ()
main = hspec $ do

-- Credit card validation

  describe "Intro.toDigits" $ do
    it "returns list of digits from a number" $ do
      toDigits 1234 `shouldBe` [1,2,3,4]
    it "returns empty list for zero" $ do
      toDigits 0 `shouldBe` []
    it "returns empty list for negative numbers" $ do
      toDigits (-17) `shouldBe` []

  describe "Intro.toDigitsRev" $ do
    it "returns list of digits from a number" $ do 
      toDigitsRev 1234 `shouldBe` [4,3,2,1]

  describe "Intro.doubleEveryOther" $ do
    it "returns every second digit doubled from the right with a 4-digit list" $ do
      doubleEveryOther [8,7,6,5] `shouldBe` [16,7,12,5]
    it "returns every second digit doubled from the right with a 3-digit list" $ do
      doubleEveryOther [1,2,3] `shouldBe` [1,4,3]
    it "returns empty list with an empty list" $ do
      doubleEveryOther [] `shouldBe` []

  describe "Intro.sumDigits" $ do
    it "returns sum of all single digits" $ do
      sumDigits [16,7,12,5] `shouldBe` 22

  describe "Intro.validate" $ do
    it "returns wheter number is valid" $ do
      validate 4012888888881881 `shouldBe` True
    it "returns wheter number is invalid" $ do
      validate 4012888888881882 `shouldBe` False

-- Tower of Hanoi

  describe "Intro.hanoi" $ do
    it "returns a correct list of moves for 2 discs" $ do
      hanoi 2 "a" "b" "c" `shouldBe` [("a","c"), ("a","b"), ("c","b")]
    it "returns a correct list of moves for 3 discs" $ do
      hanoi 3 "a" "b" "c" `shouldBe` [("a","b"), ("a","c"), ("b","c"), ("a", "b"), ("c", "a"), ("c", "b"), ("a", "b")]
