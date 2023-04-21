{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
module Blockudoku where

import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Data.Array

data Cell = Free | Filled
  deriving stock (Show, Eq)

type Coord = (Int, Int)

type Figure = Array Coord Cell

data Game = Game
  { _score :: Int,
    _board :: Array Coord Cell,
    _figures :: Array Int (Maybe Figure) }
  deriving stock (Show)

makeLenses ''Game

height, width, figuresToPlace :: Int
height = 9
width = 9
figuresToPlace = 3

initGame :: IO Game
initGame = do
  let _board =
        array ((0, 0), (height - 1, width - 1)) [((i, j), Free) | i <- [0 .. height - 1], j <- [0 .. width - 1]]
        // [
          ((0, 0), Filled),
          ((0, 1), Filled),
          ((1, 0), Filled),
          ((1, 1), Filled)
        ]
  let _figures = array (0, figuresToPlace - 1) [(i, Nothing) | i <- [0 .. figuresToPlace - 1]]
  let game = Game
        { _score = 0,
          _board = _board,
          _figures = _figures }
  return game

indicesInRow :: Int -> [(Int, Int)]
indicesInRow rowIndex = range ((rowIndex, 0), (rowIndex, width - 1))

rowIndices :: [Int]
rowIndices = [0 .. height - 1]

rowCells :: Int -> Game -> [Cell]
rowCells rowIndex game = map (\coord -> (game ^. board) ! coord) $ indicesInRow rowIndex

rows :: Game -> [[Cell]]
rows game = map (`rowCells` game) rowIndices