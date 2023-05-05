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
      it "After 4 rotations returns to an initial form" $ do
        rotateFigureNTimes 4 figure `shouldBe` figure

allTests :: TopSpec
allTests = describe "Tests" $ do
  boardTests

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions allTests