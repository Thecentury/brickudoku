{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Main (main) where

import Test.Sandwich
import Control.Monad (forM_)

import Board
  ( possibleFigures,
    rotateFigureNTimes )
import MyPrelude (mapi)
import Data.Function ((&))

boardTests :: TopSpec
boardTests = do
  forM_ (possibleFigures & mapi (,)) $ \(i, figure) -> do
    describe ("Figure " <> show i) $ do
      it "After 4 rotations should return to an initial form" $ do
        rotateFigureNTimes 4 figure `shouldBe` figure

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

allTests :: TopSpec
allTests = describe "Tests" $ do
  boardTests

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions allTests