{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
module Blockudoku where

import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Data.Array
import System.Random.Stateful
import Control.Monad (replicateM)

data Cell = Free | Filled
  deriving stock (Show, Eq)

type Coord = (Int, Int)

type Figure = Array Coord Cell

data Game = Game
  { _score :: Int,
    _board :: Figure,
    _figures :: Array Int (Maybe Figure) }
  deriving stock (Show)

makeLenses ''Game

boardHeight, boardWidth, figuresToPlaceCount :: Int
boardHeight = 9
boardWidth = 9
figuresToPlaceCount = 3

possibleFiguresData :: [[[Int]]]
possibleFiguresData =
  [
    -- Cube
    [
      [1, 1],
      [1, 1]
    ],
    -- 4-line
    [
      [1, 1, 1, 1]
    ],
    -- L
    [
      [1, 0],
      [1, 0],
      [1, 1]
    ],
    -- T
    [
      [1, 1, 1],
      [0, 1, 0]
    ],
    -- Z
    [
      [1, 1, 0],
      [0, 1, 1]
    ],
    -- S
    [
      [0, 1, 1],
      [1, 1, 0]
    ],
    -- L
    [
      [0, 1],
      [0, 1],
      [1, 1]
    ],
    -- dot
    [
      [1]
    ],
    -- 2-line
    [
      [1, 1]
    ],
    -- 3-line
    [
      [1, 1, 1]
    ],
    -- 5-line
    [
      [1, 1, 1, 1, 1]
    ],
    -- long angle
    [
      [1, 0, 0],
      [1, 0, 0],
      [1, 0, 0],
      [1, 1, 1]
    ],
    -- bracket
    [
      [1, 1],
      [1, 0],
      [1, 1]
    ],
    -- cross
    [
      [0, 1, 0],
      [1, 1, 1],
      [0, 1, 0]
    ]
  ]

mkFigure :: [[Int]] -> Figure
mkFigure idx =
  array ((0, 0), (figureHeight - 1, figureWidth - 1)) [((r, c), intToCell $ numberAt (r, c)) | r <- [0 .. figureHeight - 1], c <- [0 .. figureWidth - 1]]
  where
    figureHeight = length idx
    figureWidth = length (head idx)
    numberAt :: Coord -> Int
    numberAt (r, c) = (idx !! r) !! c
    intToCell :: Int -> Cell
    intToCell 0 = Free
    intToCell _ = Filled

possibleFigures :: [Figure]
possibleFigures = map mkFigure possibleFiguresData

rotateFigureClockwise :: Figure -> Figure
rotateFigureClockwise f =
  array ((0, 0), (newHeight - 1, newWidth - 1)) [((c, figureHeight - 1 - r), f ! (r, c)) | r <- [0 .. figureHeight - 1], c <- [0 .. figureWidth - 1]]
    where
      upperBound = snd $ bounds f
      figureWidth = snd upperBound + 1
      figureHeight = fst upperBound + 1
      newWidth = figureHeight
      newHeight = figureWidth

randomFigure :: IO Figure
randomFigure = do
  figureIndex <- uniformRM (0, length possibleFigures - 1) globalStdGen
  rotations <- uniformRM (0 :: Int, 3) globalStdGen
  let figure = possibleFigures !! figureIndex
  return $ iterate rotateFigureClockwise figure !! rotations

initGame :: IO Game
initGame = do
  let _board =
        array ((0, 0), (boardHeight - 1, boardWidth - 1)) [((i, j), Free) | i <- [0 .. boardHeight - 1], j <- [0 .. boardWidth - 1]]
        // [
          ((0, 0), Filled),
          ((0, 1), Filled),
          ((1, 0), Filled),
          ((1, 1), Filled)
        ]
  randomFigures <- replicateM figuresToPlaceCount randomFigure
  let figuresWithIndex = zip [0 .. figuresToPlaceCount - 1] $ map Just randomFigures
  let _figures = array (0, figuresToPlaceCount - 1) figuresWithIndex
  let game = Game
        { _score = 0,
          _board = _board,
          _figures = _figures }
  return game

rowCells :: Int -> Figure -> [Cell]
rowCells rowIndex f =
  [f ! (rowIndex, c) | c <- [0 .. figureWidth - 1]]
    where
      upperBound = snd $ bounds f
      figureWidth = snd upperBound + 1

rows :: Figure -> [[Cell]]
rows f = map (`rowCells` f) rowIndices where
  upperBound = snd $ bounds f
  figureHeight = fst upperBound + 1
  rowIndices = [0 .. figureHeight - 1]