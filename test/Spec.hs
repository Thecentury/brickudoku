{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Main (main) where

import Test.Sandwich
import Control.Monad (forM_)
import Linear.V2 ( R1(_x), R2(_y) )

import Board
  ( possibleFigures,
    rotateFigureNTimes, mkFigure, boardSize, possibleFigureStartCoordinates )
import MyPrelude (mapi)
import Data.Function ((&))
import Control.Lens (view)

boardTests :: TopSpec
boardTests = do
  forM_ (possibleFigures & mapi (,)) $ \(i, figure) -> do
    describe ("Figure " <> show i) $ do
      it "After 4 rotations returns to an initial form" $ do
        rotateFigureNTimes 4 figure `shouldBe` figure

  describe "Placements" $ do
    it "Single-cell figure have all board cells as possible start coordinates" $ do
      let figure = mkFigure [[1]]
      (possibleFigureStartCoordinates figure & length) `shouldBe` (boardSize * boardSize)

    it "Nine-cell horizontal bar has 9 possible start coordinates" $ do
      let figure = mkFigure [replicate boardSize 1]
      let startCoordinates = possibleFigureStartCoordinates figure
      length startCoordinates `shouldBe` 9
      all (== 0) (view _x <$> startCoordinates) `shouldBe` True

    it "Nine-cell vertical bar has 9 possible start coordinates" $ do
      let figure = mkFigure $ replicate boardSize [1]
      let startCoordinates = possibleFigureStartCoordinates figure
      length startCoordinates `shouldBe` 9
      all (== 0) (view _y <$> startCoordinates) `shouldBe` True

allTests :: TopSpec
allTests = describe "Tests" $ do
  boardTests

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions allTests