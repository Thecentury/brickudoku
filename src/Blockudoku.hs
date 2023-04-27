{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}
module Blockudoku
  ( Game,
    State(PlacingFigure, SelectingFigure),
    Figure,
    CellCoord,
    Selectable(..),
    PlacingCell(..),
    Cell(..),
    boardToPlacingCells,
    addPlacingFigure,
    UserAction(..),
    SystemAction(..),
    Action(..),
    GameEvent(..),
    board,
    figures,
    score,
    state,
    turnNumber,
    emptyFigure,
    initGame,
    figureRows,
    possibleActions ) where

import Control.Lens ( (&), makeLenses, (^.), (%~), (.~), (+~) )
import Data.Array ( (!), (//), array, bounds, Array, elems, assocs, listArray )
import System.Random.Stateful ( globalStdGen, UniformRange(uniformRM) )
import Control.Monad (replicateM)
import Data.List (findIndex, find)
import qualified Data.List as List
import Data.Maybe (mapMaybe, isJust, isNothing, catMaybes)

import MyPrelude ( mapArrayItem, mapiArray )
import qualified Data.Bifunctor
import System.Random (randomRIO)

----

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

allPlaced :: (Foldable m) => m (Maybe a) -> Bool
allPlaced = foldl (\soFar item -> soFar && isNothing item) True

--- Coordinates

data Coord = Coord { _x :: Int, _y :: Int }
  deriving stock (Show, Eq)

zeroCoord :: Coord
zeroCoord = Coord { _x = 0, _y = 0 }

data Vector = Vector { _dx :: Int, _dy :: Int }
  deriving stock (Show, Eq)

vectorUp :: Vector
vectorUp = Vector { _dx = 0, _dy = -1 }

vectorDown :: Vector
vectorDown = Vector { _dx = 0, _dy = 1 }

vectorLeft :: Vector
vectorLeft = Vector { _dx = -1, _dy = 0 }

vectorRight :: Vector
vectorRight = Vector { _dx = 1, _dy = 0 } 

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
  & map (Data.Bifunctor.second boardCellToPlacingCell)
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
  SelectingFigure | -- todo it can hold figure and its index
  PlacingFigure Figure Coord |
  GameOver
  deriving stock (Show, Eq)

-- | Ticks mark passing of time
data GameEvent = Tick
  deriving stock (Show, Eq, Ord)

data Game = Game
  { _score :: Int,
    _board :: Figure,
    _figures :: Array Int (Maybe (Selectable Figure)),
    _state :: State,
    _turnNumber :: Int,
    _autoPlay :: Bool }
  deriving stock (Show)

makeLenses ''Game

boardSize, figuresToPlaceCount :: Int
boardSize = 9
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
  pure $ mapArrayItem 0 select selectableFigures

initGame :: IO Game
initGame = do
  let _board =
        array ((0, 0), (boardSize - 1, boardSize - 1))
          [((i, j), Free) | i <- [0 .. boardSize - 1], j <- [0 .. boardSize - 1]]
  selectedFirstFigure <- randomSelectableFigures
  let game = Game
        { _score = 0,
          _board = _board,
          _figures = selectedFirstFigure,
          _state = SelectingFigure,
          _turnNumber = 1,
          _autoPlay = False }
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

nextFigureIndices :: Int -> [Int]
nextFigureIndices currentFigureIndex =
  map (`rem` figuresToPlaceCount) [currentFigureIndex + 1, currentFigureIndex + 2]

previousFigureIndices :: Int -> [Int]
previousFigureIndices currentFigureIndex =
  map (`rem` figuresToPlaceCount) [figuresToPlaceCount + currentFigureIndex - 1, figuresToPlaceCount + currentFigureIndex - 2]

tryFindNextFigureToSelect :: Game -> (Int -> [Int]) -> Maybe Int
tryFindNextFigureToSelect g nextIndices = do
  currentIndex <- selectedFigureIndex g
  let nexts = map (\i -> (i, (g ^. figures) ! i)) $ nextIndices currentIndex
  fst <$> find (\(_, f) -> canBeSelected f) nexts
  where
    canBeSelected :: Maybe (Selectable Figure) -> Bool
    canBeSelected Nothing = False
    canBeSelected (Just (Selected _)) = False
    canBeSelected (Just (NotSelected f)) = canBePlacedToBoardAtSomePoint f $ g ^. board

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

----

-- | Frees cells by specified coordinates
freeAllCells :: Figure -> [CellCoord] -> Figure
freeAllCells fig coords =
  fig // fmap (, Free) coords

-- | Determines whether all cells by specified coordinates are filled
allCellsAreFilled :: Figure -> [CellCoord] -> Bool
allCellsAreFilled fig coords =
  coords & fmap (fig !) & all (== Filled)

full9Ranges :: [[CellCoord]]
full9Ranges = allHorizontal ++ allVertical ++ squares where
  horizontal r  = fmap (r, ) all9
  vertical   c  = fmap (, c) all9
  allHorizontal = fmap horizontal all9
  allVertical   = fmap vertical all9
  all9          = [0 .. boardSize - 1]
  square startRow startColumn = [(startRow + r, startColumn + c) | r <- [0..2], c <- [0..2]]
  squares       = [square (r * 3) (c * 3) | r <- [0..2], c <- [0..2]]

removeFilledRanges :: Figure -> Figure
removeFilledRanges fig =
  foldl freeAllCells fig fullRanges where
    fullRanges = filter (allCellsAreFilled fig) full9Ranges

possibleFigureStartCoordinates :: Figure -> [Coord]
possibleFigureStartCoordinates fig =
  [Coord { _x = x, _y = y } | x <- [0 .. boardSize - figureWidth - 1], y <- [0 .. boardSize - figureHeight - 1]] where
    figureBounds = snd $ bounds fig
    (figureWidth, figureHeight) = figureBounds

canBePlacedToBoardAtSomePoint :: Figure -> Figure -> Bool
canBePlacedToBoardAtSomePoint fig b =
  List.any (\coord -> tryPlaceFigure fig coord b & isJust) starts where
    starts = possibleFigureStartCoordinates fig

---- Commands

data UserAction =
  SelectNextFigure |
  SelectPreviousFigure |
  MoveFigureRight |
  MoveFigureLeft |
  MoveFigureDown |
  MoveFigureUp |
  StartPlacingFigure |
  CancelPlacingFigure |
  PlaceFigure
  deriving stock (Eq, Show)

data SystemAction =
  RestartGame |
  NextAutoPlayTurn |
  ToggleAutoPlay
  deriving stock (Eq, Show)

data Action =
  UserAction UserAction |
  SystemAction SystemAction
  deriving stock (Eq, Show)

onlyUserAction :: Action -> Maybe UserAction
onlyUserAction (UserAction a) = Just a
onlyUserAction _ = Nothing

userActionProbability :: UserAction -> Int
userActionProbability PlaceFigure = 30
userActionProbability CancelPlacingFigure = 1
userActionProbability _ = 10

restartGameAction :: IO (Maybe (Action, Game))
restartGameAction = (\g -> Just (SystemAction RestartGame, g)) <$> initGame

toggleAutoPlayAction :: Game -> Maybe (Action, Game)
toggleAutoPlayAction game =
  Just (SystemAction ToggleAutoPlay, game & autoPlay %~ not)

nextAutoPlayTurnAction :: Game -> Bool -> IO (Maybe (Action, Game))
nextAutoPlayTurnAction game generateAutoPlay = do
  if game ^. autoPlay && generateAutoPlay then
    nextAction
  else
    pure Nothing
  where
    nextAction :: IO (Maybe (Action, Game))
    nextAction = do
      actions <- possibleActionsImpl game False
      let applicableActions = mapMaybe (\(a, g) -> (, g) <$> onlyUserAction a) actions
      if null applicableActions then
        pure Nothing
      else do
        (_, game') <- randomElement $ actionsAccordingToProbability applicableActions
        pure $ Just (SystemAction NextAutoPlayTurn, game')

actionsAccordingToProbability :: [(UserAction, a)] -> [(UserAction, a)]
actionsAccordingToProbability = concatMap (\(action, game) -> replicate (userActionProbability action) (action, game))

randomElement :: [a] -> IO a
randomElement list = do
  randomIndex <- randomRIO (0, length list - 1)
  pure $ list !! randomIndex

possibleActionsImpl :: Game -> Bool -> IO [(Action, Game)]
possibleActionsImpl game generateAutoPlay = do
  case game ^. state of
    SelectingFigure -> actions where
      actions = do
        newGame <- restartGameAction
        autoPlayTurn <- nextAutoPlayTurnAction game generateAutoPlay
        pure $ catMaybes
          [
            moveFigure (UserAction SelectNextFigure) nextFigureIndices,
            moveFigure (UserAction SelectPreviousFigure) previousFigureIndices,
            startPlacing,
            newGame,
            toggleAutoPlayAction game,
            autoPlayTurn
          ]
      setSelected indexToSelect = mapiArray (\i x -> if i == indexToSelect then select x else deselect x)

      moveFigure action nextIndices = do
        nextIx <- tryFindNextFigureToSelect game nextIndices
        let game' = game & figures %~ setSelected nextIx
        pure (action, game')

      startPlacing = do
        selectedFigure_ <- selectedFigure game
        let game' = game & state .~ PlacingFigure selectedFigure_ zeroCoord
        pure (UserAction StartPlacingFigure, game')

    PlacingFigure fig coord -> actions where
      board_ = game ^. board

      tryMove :: Vector -> Action -> Maybe (Action, Game)
      tryMove movement action = do
        newCoord <- tryMoveFigure board_ fig coord movement
        let game' = game & state .~ PlacingFigure fig newCoord
        pure (action, game')

      place :: IO (Maybe (Action, Game))
      place = do
        case tryPlaceFigure fig coord board_ of
          Nothing -> pure Nothing
          Just newBoard -> do
            let newFigures = game ^. figures & fmap markSelectedAsPlaced
            (newFigures2, turnIncrement) <-
              if allPlaced newFigures then
                fmap (, 1) randomSelectableFigures
              else
                pure (selectFirstSelectableFigure newFigures, 0)
            let g' =
                  state .~ SelectingFigure
                  $ figures .~ newFigures2
                  $ board .~ removeFilledRanges newBoard
                  $ turnNumber +~ turnIncrement
                  $ game
            pure $ Just (UserAction PlaceFigure, g')

      actions :: IO [(Action, Game)]
      actions = do
        place' <- place
        newGame <- restartGameAction
        autoPlayTurn <- nextAutoPlayTurnAction game generateAutoPlay
        pure $ catMaybes [
            tryMove vectorRight $ UserAction MoveFigureRight,
            tryMove vectorLeft $ UserAction MoveFigureLeft,
            tryMove vectorDown $ UserAction MoveFigureDown,
            tryMove vectorUp $ UserAction MoveFigureUp,
            place',
            newGame,
            toggleAutoPlayAction game,
            autoPlayTurn,
            Just (UserAction CancelPlacingFigure, game & state .~ SelectingFigure)
          ]
    GameOver -> do
      newGame <- restartGameAction
      pure $ catMaybes [newGame]

possibleActions :: Game -> IO [(Action, Game)]
possibleActions game = possibleActionsImpl game True
