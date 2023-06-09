{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TupleSections #-}

module Board where

import Control.Lens ( (&), (^.) )
import Data.Array ( (//), array, bounds, Array, assocs )
import System.Random.Stateful ( UniformRange(uniformRM), StatefulGen )
import Data.Maybe (mapMaybe)
import Linear.V2 (V2(..), _x, _y)
import MyPrelude ( (!), width2d, height2d )
import GHC.Stack (HasCallStack)

import Primitives ( Cell(..), Coord, RangeKind(..), zeroCoord )

--------------------------------------------------------------------------------

type Figure = Array Coord Cell

figureCellCoords :: Figure -> [Coord]
figureCellCoords = fmap fst . filter (\(_, e) -> e == Filled) . assocs

type Board = Figure

validateFigureStartCoord :: Board -> Figure -> Coord -> Maybe Coord
validateFigureStartCoord board figure coord =
  let
    figureSize = snd $ bounds figure
    (boardTopLeft, boardBottomRight) = bounds board
    topLeftWithinBoard =
      coord ^. _x >= boardTopLeft ^. _x &&
      coord ^. _y >= boardTopLeft ^. _y
    bottomRightWithinBoard =
      coord ^. _x + figureSize ^. _x <= boardBottomRight ^. _x &&
      coord ^. _y + figureSize ^. _y <= boardBottomRight ^. _y
  in
    if topLeftWithinBoard && bottomRightWithinBoard then
      Just coord
    else
      Nothing

tryMoveFigure :: Board -> Figure -> Coord -> Coord -> Maybe Coord
tryMoveFigure board figure coord vector = validateFigureStartCoord board figure (coord + vector)

----

boardSize, figuresToPlaceCount :: Int
boardSize = 9
figuresToPlaceCount = 3

figureCenter :: Figure -> Coord
figureCenter f = V2 (mid $ width2d f) (mid $ height2d f) where
  mid :: Int -> Int
  mid dim | odd dim = dim `div` 2     -- Mid of width 3 is coord 1. 
  mid dim           = dim `div` 2 - 1

centerFigureTopLeft :: Board -> Figure -> Coord
centerFigureTopLeft b f = figureCenter b - figureCenter f

----

-- | Frees cells by specified coordinates
freeAllCells :: Board -> [Coord] -> Board
freeAllCells b coords =
  b // fmap (, Free) coords

-- | Determines whether all cells by specified coordinates are filled
allCellsAreFilled :: HasCallStack => Figure -> [Coord] -> Bool
allCellsAreFilled fig coords =
  coords & fmap (fig !) & all (== Filled)

full9Ranges :: [(RangeKind, [Coord])]
full9Ranges = allHorizontal ++ allVertical ++ squares where
  horizontal y  = (Horizontal, fmap (`V2` y) all9)
  vertical   x  = (Vertical, fmap (V2 x) all9)
  allHorizontal = fmap horizontal all9
  allVertical   = fmap vertical all9
  all9          = [0 .. boardSize - 1]
  square startRow startColumn = (Square, [V2 (startColumn + x) (startRow + y) | y <- [0..2], x <- [0..2]])
  squares       = [square (r * 3) (c * 3) | r <- [0..2], c <- [0..2]]

-- | Finds ranges of cells that will be freed.
rangesToBeFreed :: HasCallStack => Board -> [(RangeKind, [Coord])]
rangesToBeFreed b = filter (\(_, range) -> allCellsAreFilled b range) full9Ranges

removeFilledRanges :: HasCallStack => Board -> Board
removeFilledRanges b = foldl (\b' (_, range) -> freeAllCells b' range) b $ rangesToBeFreed b

possibleFigureStartCoordinates :: Figure -> [Coord]
possibleFigureStartCoordinates fig =
  [V2 x y | y <- [0 .. boardSize - figureHeight - 1], x <- [0 .. boardSize - figureWidth - 1]] where
    (_, V2 figureWidth figureHeight) = bounds fig

pointsWhereFigureCanBePlaced :: HasCallStack => Figure -> Board -> [Coord]
pointsWhereFigureCanBePlaced fig b =
  mapMaybe (\coord -> coord <$ tryPlaceFigure fig coord b) $ possibleFigureStartCoordinates fig

canBePlacedToBoardAtSomePoint :: HasCallStack => Figure -> Board -> Bool
canBePlacedToBoardAtSomePoint fig b =
  not . null $ pointsWhereFigureCanBePlaced fig b

tryPlaceFigure :: HasCallStack => Figure -> Coord -> Board -> Maybe Figure
tryPlaceFigure figure figureCoord board =
  tryPlace board figureCells
  where
    figureCells = (+ figureCoord) <$> figureCellCoords figure

    tryPlace :: Figure -> [Coord] -> Maybe Figure
    tryPlace b [] = Just b
    tryPlace b (coord : coords) =
      case b ! coord of
        Free -> tryPlace (b // [(coord, Filled)]) coords
        Filled -> Nothing

--- Figures generation ---

possibleFiguresData :: [[[Int]]]
possibleFiguresData =
  [
    -- Cube
    [
      [1, 1],
      [1, 1]
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
    -- 4-line
    [
      [1, 1, 1, 1]
    ],
    -- 5-line
    [
      [1, 1, 1, 1, 1]
    ],
    -- long angle
    [
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
    ],
    -- 2-diagonal
    [
      [1, 0],
      [0, 1]
    ],
    -- 3-diagonal
    [
      [1, 0, 0],
      [0, 1, 0],
      [0, 0, 1]
    ]
  ]

mkFigure :: [[Int]] -> Figure
mkFigure idx =
  array
    (zeroCoord, V2 (figureWidth - 1) (figureHeight - 1))
    [(V2 x y, intToCell $ numberAt (V2 x y)) | y <- [0 .. figureHeight - 1], x <- [0 .. figureWidth - 1]]
  where
    figureHeight = length idx
    figureWidth = length (head idx)

    numberAt :: Coord -> Int
    numberAt (V2 x y) = (idx !! y) !! x

    intToCell :: Int -> Cell
    intToCell 0 = Free
    intToCell _ = Filled

possibleFigures :: [Figure]
possibleFigures = map mkFigure possibleFiguresData

emptyFigure :: Figure
emptyFigure = mkFigure [[0]]

rotateFigureClockwise :: HasCallStack => Figure -> Figure
rotateFigureClockwise f =
  array
    (zeroCoord, V2 (newWidth - 1) (newHeight - 1))
    [(V2 (figureHeight - 1 - y) x, f ! V2 x y) | y <- [0 .. figureHeight - 1], x <- [0 .. figureWidth - 1]]
    where
      figureWidth = width2d f
      figureHeight = height2d f
      newWidth = figureHeight
      newHeight = figureWidth

rotateFigureNTimes :: HasCallStack => Int -> Figure -> Figure
rotateFigureNTimes n f = iterate rotateFigureClockwise f !! n

randomRawFigure :: (HasCallStack, StatefulGen g m) => g -> m Figure
randomRawFigure gen = do
  ix <- uniformRM (0, length possibleFigures - 1) gen
  rotations <- uniformRM (0 :: Int, 3) gen
  let figure = possibleFigures !! ix
  pure $ rotateFigureNTimes rotations figure