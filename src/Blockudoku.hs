{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Blockudoku where

import Control.Lens hiding ((<|), (|>), (:>), (:<), index)
import Data.Array ( (!), (//), array, bounds, Array, elems, assocs, listArray )
import System.Random.Stateful ( globalStdGen, UniformRange(uniformRM) )
import Control.Monad (replicateM)
import Data.List (findIndex)
import Data.Maybe (mapMaybe, fromMaybe)

import MyPrelude

--

data Cell =
  Free | Filled
  deriving stock (Show, Eq)

data PlacingCell =
  PlacingFree |
  PlacingFilled |
  PlacingCanPlaceFullFigure |
  PlacingCanPlaceButNotFullFigure |
  PlacingCannotPlace
  deriving stock (Show, Eq)

-- todo is it needed?
data CanBePlaced = CanBePlaced | CanNotBePlaced
  deriving stock (Show, Eq)

data Selectable a =
  Selected a |
  NotSelected a
  deriving stock (Show, Eq)

select :: Maybe (Selectable a) -> Maybe (Selectable a)
select (Just (Selected a)) = Just $ Selected a
select (Just (NotSelected a)) = Just $ Selected a
select Nothing = Nothing

deselect :: Maybe (Selectable a) -> Maybe (Selectable a)
deselect (Just (Selected a)) = Just $ NotSelected a
deselect (Just (NotSelected a)) = Just $ NotSelected a
deselect Nothing = Nothing

markSelectedAsPlaced :: Maybe (Selectable a) -> Maybe (Selectable a)
markSelectedAsPlaced (Just (Selected _)) = Nothing
markSelectedAsPlaced (Just (NotSelected a)) = Just $ NotSelected a
markSelectedAsPlaced Nothing = Nothing

allPlaced :: (Foldable m, Eq a) => m (Maybe a) -> Bool
allPlaced a = foldl (\soFar item -> soFar && item == Nothing) True a

--- Coordinates

data Coord = Coord { _x :: Int, _y :: Int }
  deriving stock (Show, Eq)

coordToCellCoord :: Coord -> CellCoord
coordToCellCoord (Coord x y) = (y, x)

data Vector = Vector { _dx :: Int, _dy :: Int }
  deriving stock (Show, Eq)

data Direction = DirUp | DirDown | DirLeft | DirRight
  deriving stock (Show, Eq)

directionToVector :: Direction -> Vector
directionToVector DirUp = Vector { _dx = 0, _dy = -1 }
directionToVector DirDown = Vector { _dx = 0, _dy = 1 }
directionToVector DirLeft = Vector { _dx = -1, _dy = 0 }
directionToVector DirRight = Vector { _dx = 1, _dy = 0 }

addVector :: Coord -> Vector -> Coord
addVector (Coord x y) (Vector dx dy) = Coord (x + dx) (y + dy)

---

type CellCoord = (Int, Int)

row :: CellCoord -> Int
row = fst

col :: CellCoord -> Int
col = snd

type Figure = Array CellCoord Cell

type PlacingCellsFigure = Array CellCoord PlacingCell

tryMoveFigure :: Figure -> Figure -> Coord -> Vector -> Maybe Coord
tryMoveFigure board figure coord vector =
  let
    newCoord = addVector coord vector
    figureSize = snd $ bounds figure
    boardBounds = bounds board
    boardTopLeft = fst boardBounds
    boardBottomRight = snd boardBounds
    topLeftWithinBoard =
      newCoord._x >= col boardTopLeft &&
      newCoord._y >= row boardTopLeft
    bottomRightWithinBoard =
      newCoord._x + col figureSize <= col boardBottomRight &&
      newCoord._y + row figureSize <= row boardBottomRight
  in
    if topLeftWithinBoard && bottomRightWithinBoard then
      Just newCoord
    else
      Nothing

boardCellToPlacingCell :: Cell -> PlacingCell
boardCellToPlacingCell Free = PlacingFree
boardCellToPlacingCell Filled = PlacingFilled

boardToPlacingCells :: Figure -> Array CellCoord PlacingCell
boardToPlacingCells board =
  board
  & assocs
  & map (\(coord, cell) -> (coord, boardCellToPlacingCell cell))
  & array (bounds board)

tryPlaceFigure :: Figure -> Coord -> Figure -> Maybe Figure
tryPlaceFigure figure figureCoord board =
  let
    figureCells =
      figure
      & assocs
      & mapMaybe (\(coord, cell) -> if cell == Filled then Just $ newCoord coord else Nothing)
  in
    tryPlace board figureCells
  where
    newCoord :: CellCoord -> CellCoord
    newCoord (r, c) = (r + figureCoord._y, c + figureCoord._x)

    tryPlace :: Figure -> [CellCoord] -> Maybe Figure
    tryPlace b [] = Just b
    tryPlace b (coord : coords) =
      case b ! coord of
        Free -> tryPlace (b // [(coord, Filled)]) coords
        Filled -> Nothing

addPlacingFigure :: Figure -> Coord -> Figure -> PlacingCellsFigure
addPlacingFigure figure figureCoord board =
  placingBoard // figureCells
  where
    placingBoard = boardToPlacingCells board

    figureCells =
      figure
      & assocs
      & map (\(coord, cell) -> (newCoord coord, figureCell (newCoord coord) cell))

    newCoord :: CellCoord -> CellCoord
    newCoord (r, c) = (r + figureCoord._y, c + figureCoord._x)

    canPlaceFullFigure =
      figure
      & assocs
      & mapMaybe (\(coord, cell) -> if cell == Filled then Just $ newCoord coord else Nothing)
      & all (\coord -> board ! coord == Free)

    figureCell :: CellCoord -> Cell -> PlacingCell
    figureCell boardCoord figCell =
      case (board ! boardCoord, figCell, canPlaceFullFigure) of
        (Filled, Filled, _) -> PlacingCannotPlace
        (Filled, Free, _) -> PlacingFilled
        (Free, Filled, True) -> PlacingCanPlaceFullFigure
        (Free, Filled, False) -> PlacingCanPlaceButNotFullFigure
        (Free, Free, _) -> PlacingFree

data State =
  SelectingFigure |
  PlacingFigure Figure Coord |
  GameOver
  deriving stock (Show, Eq)

data Game = Game
  { _score :: Int,
    _board :: Figure,
    _figures :: Array Int (Maybe (Selectable Figure)),
    _state :: State }
  deriving stock (Show)

makeLenses ''Game

boardHeight, boardWidth, figuresToPlaceCount :: Int
boardHeight = 9
boardWidth = 9
figuresToPlaceCount = 3

--- Figures generation ---

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

    numberAt :: CellCoord -> Int
    numberAt (r, c) = (idx !! r) !! c

    intToCell :: Int -> Cell
    intToCell 0 = Free
    intToCell _ = Filled

possibleFigures :: [Figure]
possibleFigures = map mkFigure possibleFiguresData

emptyFigure :: Figure
emptyFigure = mkFigure [[0]]

rotateFigureClockwise :: Figure -> Figure
rotateFigureClockwise f =
  array ((0, 0), (newHeight - 1, newWidth - 1)) [((c, figureHeight - 1 - r), f ! (r, c)) | r <- [0 .. figureHeight - 1], c <- [0 .. figureWidth - 1]]
    where
      upperBound = snd $ bounds f
      figureWidth = snd upperBound + 1
      figureHeight = fst upperBound + 1
      newWidth = figureHeight
      newHeight = figureWidth

randomRawFigure :: IO Figure
randomRawFigure = do
  figureIndex <- uniformRM (0, length possibleFigures - 1) globalStdGen
  rotations <- uniformRM (0 :: Int, 3) globalStdGen
  let figure = possibleFigures !! figureIndex
  return $ iterate rotateFigureClockwise figure !! rotations

---

randomSelectableFigures :: IO (Array Int (Maybe (Selectable Figure)))
randomSelectableFigures = do
  rawFigures <- replicateM figuresToPlaceCount randomRawFigure
  let selectableFigures = listArray (0, figuresToPlaceCount - 1) $ map (Just . NotSelected) rawFigures
  -- Select first figure
  pure $ mapArrayItem 0 select $ selectableFigures

initGame :: IO Game
initGame = do
  let _board =
        array ((0, 0), (boardHeight - 1, boardWidth - 1))
          [((i, j), Free) | i <- [0 .. boardHeight - 1], j <- [0 .. boardWidth - 1]]
  selectedFirstFigure <- randomSelectableFigures
  let game = Game
        { _score = 0,
          _board = _board,
          _figures = selectedFirstFigure,
          _state = SelectingFigure }
  return game

rowCells :: Int -> Array CellCoord a -> [a]
rowCells rowIndex f =
  [f ! (rowIndex, c) | c <- [0 .. figureWidth - 1]]
    where
      upperBound  = snd $ bounds f
      figureWidth = snd upperBound + 1

figureRows :: Array CellCoord a -> [[a]]
figureRows f = map (`rowCells` f) rowIndices where
  upperBound = snd $ bounds f
  figureHeight = fst upperBound + 1
  rowIndices = [0 .. figureHeight - 1]

--- Event handling ---

selectedFigureIndex :: Game -> Maybe Int
selectedFigureIndex game =
  game ^. figures & elems & findIndex isSelected
    where
      isSelected (Just (Selected _)) = True
      isSelected _ = False

selectedFigure :: Game -> Maybe Figure
selectedFigure game =
  case selectedFigureIndex game >>= \i -> (game ^. figures) ! i of
    Just (Selected f) -> Just f
    _ -> Nothing

nextSelectedFigureIndex :: Int -> Maybe Int
nextSelectedFigureIndex currentFigureIndex =
  if currentFigureIndex == figuresToPlaceCount - 1
    then Nothing
    else Just $ currentFigureIndex + 1

previousSelectedFigureIndex :: Int -> Maybe Int
previousSelectedFigureIndex currentFigureIndex =
  if currentFigureIndex == 0
    then Nothing
    else Just $ currentFigureIndex - 1

-- todo not all figures may be selectable
selectFirstSelectableFigure :: Array Int (Maybe (Selectable a)) -> Array Int (Maybe (Selectable a))
selectFirstSelectableFigure figs =
  go 0
  where
    next index f =
      let (_, maxIndex) = bounds figs
      in if index < maxIndex then
        f (index + 1)
      else
        figs

    go index =
      case figs ! index of
        Nothing -> next index go
        Just (Selected _) -> figs
        Just (NotSelected a) -> figs // [(index, Just (Selected a))]

-- todo not all figures may be selectable
selectNextFigure :: (Int -> Maybe Int) -> Game -> Game
selectNextFigure calculateNextIndex game =
  maybe game (\nextIndex -> game & figures %~ setSelected nextIndex) (selectedFigureIndex game >>= calculateNextIndex) where
    setSelected indexToSelect a =
      a
      & assocs
      & map (\(i, x) -> if i == indexToSelect then (i, select x) else (i, deselect x))
      & array (bounds a)

startPlacingFigure :: Game -> Game
startPlacingFigure game =
  maybe game (\f -> game & state .~ PlacingFigure f (Coord { _x = 0, _y = 0 })) (selectedFigure game)

cancelPlacingFigure :: Game -> Game
cancelPlacingFigure game =
  case game ^. state of
    PlacingFigure _ _ -> game & state .~ SelectingFigure
    _ -> game

---

movePlacingFigure :: Game -> Direction -> Game
movePlacingFigure game direction =
  case game ^. state of
    PlacingFigure figure coord ->
      let
        newCoord = fromMaybe coord $ tryMoveFigure game._board figure coord (directionToVector direction)
      in
        state .~ PlacingFigure figure newCoord $ game
    _ -> game

placeFigure :: Game -> IO Game
placeFigure game =
  case game ^. state of
    PlacingFigure figure coord ->
      case tryPlaceFigure figure coord $ game ^. board of
        Just newBoard -> do
          let newFigures = game ^. figures & fmap markSelectedAsPlaced
          newFigures2 <-
            if allPlaced newFigures then
              randomSelectableFigures
            else
              pure $ selectFirstSelectableFigure newFigures
          pure $ state .~ SelectingFigure $ figures .~ newFigures2 $ board .~ newBoard $ game
        Nothing -> pure game
    _ -> pure game

--placeFigure :: Game -> Game
--placeFigure game =
--  case game ^. state of
--    PlacingFigure figure coord ->
--      let
--        newBoard = placeFigureOnBoard game._board figure coord
--        newScore = game._score + figureScore figure
--        newFigures = game._figures & assocs & map (\(i, x) -> if i == fromJust (selectedFigureIndex game) then (i, deselect x) else (i, x)) & array (bounds game._figures)
--        newGame = game & board .~ newBoard & score .~ newScore & figures .~ newFigures
--      in
--        if isNothing (selectedFigureIndex newGame)
--          then newGame & state .~ GameOver
--          else newGame & state .~ SelectingFigure
--    _ -> game