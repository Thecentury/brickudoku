{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Sandwich

basic :: TopSpec
basic = do
  describe "Arithmetic" $ do
    it "adds" $ do
      (2 + 2) `shouldBe` 4
      (2 + 3) `shouldBe` 5

    it "subtracts" $ do
      (3 - 2) `shouldBe` 0
      warn "This test might not be right..."

  describe "Strings" $
    it "concatenates" $
      ("abc" <> "def") `shouldBe` "abcdef"

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions basic