{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Blockudoku
  ( Game,
    GameState(..),
    Figure,
    CellCoord,
    PlacingCell(..),
    Cell(..),
    FigureInSelection,
    FigureToPlace(..),
    FigureToPlaceKind(..),
    figureInSelection,
    cellsToDisplay,
    isGameOver,
    UserAction(..),
    SystemAction(..),
    Action(..),
    GameEvent(..),
    figuresToPlace,
    board,
    score,
    turnNumber,
    emptyFigure,
    initGame,
    figureRows,
    possibleActions ) where

import Control.Lens ( (&), makeLenses, (^.), (%~), (.~), (+~) )
import Data.Array ( (//), array, bounds, Array, assocs, listArray, elems )
import System.Random.Stateful ( globalStdGen, UniformRange(uniformRM) )
import Control.Monad (replicateM, join)
import Data.List (find)
import qualified Data.List as List
import Data.Maybe (mapMaybe, isJust, isNothing, catMaybes)
import Linear.V2 (V2(..), _x, _y)
import MyPrelude ( mapiArray, (!) )
import qualified Data.Bifunctor
import System.Random (randomRIO)
import GHC.Stack (HasCallStack)

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

allPlaced :: [Maybe a] -> Bool
allPlaced = all isNothing

--- Coordinates

type Coord = V2 Int

zeroCoord :: Coord
zeroCoord = V2 0 0

vectorUp :: Coord
vectorUp = V2 0 (-1)

vectorDown :: Coord
vectorDown = V2 0 1

vectorLeft :: Coord
vectorLeft = V2 (-1) 0

vectorRight :: Coord
vectorRight = V2 1 0 

---

-- todo use some V2 type from other brick applications?
type CellCoord = (Int, Int)

row :: CellCoord -> Int
row = fst

col :: CellCoord -> Int
col = snd

type Figure = Array CellCoord Cell

type PlacingCellsFigure = Array CellCoord PlacingCell

tryMoveFigure :: Figure -> Figure -> Coord -> Coord -> Maybe Coord
tryMoveFigure board figure coord vector =
  let
    newCoord = coord + vector
    figureSize = snd $ bounds figure
    boardBounds = bounds board
    boardTopLeft = fst boardBounds
    boardBottomRight = snd boardBounds
    topLeftWithinBoard =
      newCoord ^. _x >= col boardTopLeft &&
      newCoord ^. _y >= row boardTopLeft
    bottomRightWithinBoard =
      newCoord ^. _x + col figureSize <= col boardBottomRight &&
      newCoord ^. _y + row figureSize <= row boardBottomRight
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

markFigureAsPlaced :: FigureInSelection -> Array FigureIndex (Maybe FigureInSelection) -> Array FigureIndex (Maybe FigureInSelection)
markFigureAsPlaced figureInSelection figures =
  figures // [(_figureIndex figureInSelection, Nothing)]

tryPlaceFigure :: HasCallStack => Figure -> Coord -> Figure -> Maybe Figure
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
    newCoord (r, c) = (r + figureCoord^._y, c + figureCoord^._x)

    tryPlace :: Figure -> [CellCoord] -> Maybe Figure
    tryPlace b [] = Just b
    tryPlace b (coord : coords) =
      case b ! coord of
        Free -> tryPlace (b // [(coord, Filled)]) coords
        Filled -> Nothing

addPlacingFigure :: HasCallStack => Figure -> Coord -> Figure -> PlacingCellsFigure
addPlacingFigure figure figureCoord board =
  placingBoard // figureCells
  where
    placingBoard = boardToPlacingCells board

    figureCells =
      figure
      & assocs
      & map (\(coord, cell) -> (newCoord coord, figureCell (newCoord coord) cell))

    newCoord :: CellCoord -> CellCoord
    newCoord (r, c) = (r + figureCoord^._y, c + figureCoord^._x)

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

type FigureIndex = Int

-- todo rename, not always relates to a selection
data FigureInSelection = FigureInSelection
  { _figureInSelection :: Figure,
    _figureIndex :: FigureIndex }
  deriving stock (Show, Eq)

data FigureToPlaceKind =
  -- | Not selected figure that can be placed to a board
  CanBePlaced |
  -- | Currently selected figure
  Selected |
  -- | Currently selected figure in a placing mode
  SelectedPlacing |
  -- | Figure that cannot be placed to a board
  CannotBePlaced
  deriving stock (Show, Eq)

data FigureToPlace a = FigureToPlace
  { _figureToPlace :: a,
    _figureKind :: FigureToPlaceKind }
    deriving stock (Show, Eq)

makeLenses ''FigureInSelection

data GameState =
  SelectingFigure FigureInSelection |
  PlacingFigure FigureInSelection Coord |
  GameOver
  deriving stock (Show, Eq)

-- | Ticks mark passing of time
data GameEvent = Tick
  deriving stock (Show, Eq, Ord)

data Game = Game
  { _score :: Int,
    _board :: Figure,
    _figures :: Array Int (Maybe FigureInSelection),
    _state :: GameState,
    _turnNumber :: Int,
    _autoPlay :: Bool }
  deriving stock (Show)

makeLenses ''Game

isGameOver :: Game -> Bool
isGameOver game = case game ^. state of
  GameOver -> True
  _ -> False

cellsToDisplay :: Game -> PlacingCellsFigure
cellsToDisplay game = case game ^. Blockudoku.state of
  PlacingFigure figure coord -> addPlacingFigure (figure ^. figureInSelection) coord $ game ^. board
  _ -> game ^. board & boardToPlacingCells

figuresToPlace :: Game -> [Maybe (FigureToPlace FigureInSelection)]
figuresToPlace game =
  game
  & _figures
  & elems
  & map (fmap (\fig -> FigureToPlace fig $ kind fig)) where
    kind :: FigureInSelection -> FigureToPlaceKind
    kind fig =
      case game ^. state of
        GameOver -> CannotBePlaced
        SelectingFigure selectedFigure -> canBePlaced selectedFigure fig Selected
        PlacingFigure selectedFigure _ -> canBePlaced selectedFigure fig SelectedPlacing
    canBePlaced :: FigureInSelection -> FigureInSelection -> FigureToPlaceKind -> FigureToPlaceKind
    canBePlaced selectedFigure fig selectedMode
      | fig == selectedFigure = selectedMode
      | canBePlacedToBoardAtSomePoint (fig ^. figureInSelection) (game ^. board) = CanBePlaced
      | otherwise = CannotBePlaced      

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

rotateFigureClockwise :: HasCallStack => Figure -> Figure
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
  ix <- uniformRM (0, length possibleFigures - 1) globalStdGen
  rotations <- uniformRM (0 :: Int, 3) globalStdGen
  let figure = possibleFigures !! ix
  return $ iterate rotateFigureClockwise figure !! rotations

---

randomFigures :: IO (Array Int FigureInSelection)
randomFigures = do
  rawFigures <- replicateM figuresToPlaceCount randomRawFigure
  pure $ mapiArray (flip FigureInSelection) $ listArray (0, figuresToPlaceCount - 1) rawFigures

initGame :: IO Game
initGame = do
  let _board =
        array ((0, 0), (boardSize - 1, boardSize - 1))
          [((i, j), Free) | i <- [0 .. boardSize - 1], j <- [0 .. boardSize - 1]]
  boardFigures <- randomFigures
  let justFigures = Just <$> boardFigures
  let game = Game
        { _score = 0,
          _board = _board,
          _figures = justFigures,
          _state = SelectingFigure $ boardFigures ! 0,
          _turnNumber = 1,
          _autoPlay = False }
  return game

rowCells :: HasCallStack => Int -> Array CellCoord a -> [a]
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

nextFigureIndices :: FigureIndex -> [FigureIndex]
nextFigureIndices currentFigureIndex =
  map (`rem` figuresToPlaceCount) [currentFigureIndex + 1, currentFigureIndex + 2]

previousFigureIndices :: FigureIndex -> [FigureIndex]
previousFigureIndices currentFigureIndex =
  map (`rem` figuresToPlaceCount) [figuresToPlaceCount + currentFigureIndex - 1, figuresToPlaceCount + currentFigureIndex - 2]

tryFindNextFigureToSelect :: HasCallStack => Figure -> Array FigureIndex (Maybe FigureInSelection) -> [FigureIndex] -> Maybe FigureInSelection
tryFindNextFigureToSelect b figs nextIndices =
  join $ find canBeSelected nexts where
    canBeSelected :: Maybe FigureInSelection -> Bool
    canBeSelected Nothing = False
    canBeSelected (Just f) = canBePlacedToBoardAtSomePoint (f ^. figureInSelection) b

    nexts :: [Maybe FigureInSelection]
    nexts = (figs !) <$> nextIndices

----

-- | Frees cells by specified coordinates
freeAllCells :: Figure -> [CellCoord] -> Figure
freeAllCells fig coords =
  fig // fmap (, Free) coords

-- | Determines whether all cells by specified coordinates are filled
allCellsAreFilled :: HasCallStack => Figure -> [CellCoord] -> Bool
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
  [V2 x y | x <- [0 .. boardSize - figureWidth - 1], y <- [0 .. boardSize - figureHeight - 1]] where
    (figureHeight, figureWidth) = snd $ bounds fig

canBePlacedToBoardAtSomePoint :: HasCallStack => Figure -> Figure -> Bool
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
  -- store RNG in the Game, use randomR
  randomIndex <- randomRIO (0, length list - 1)
  pure $ list !! randomIndex

possibleActionsImpl :: HasCallStack => Game -> Bool -> IO [(Action, Game)]
possibleActionsImpl game generateAutoPlay = do
  case game ^. state of
    SelectingFigure figure -> actions where
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

      moveFigure :: Action -> (FigureIndex -> [FigureIndex]) -> Maybe (Action, Game)
      moveFigure action calculateNextIndices = do
        let nextIndices = calculateNextIndices $ figure ^. figureIndex
        nextFigure <- tryFindNextFigureToSelect (game ^. board) (game ^. figures) nextIndices
        let game' = game & state .~ SelectingFigure nextFigure
        pure (action, game')

      startPlacing = do
        let game' = game & state .~ PlacingFigure figure zeroCoord
        pure (UserAction StartPlacingFigure, game')

    PlacingFigure figure coord -> actions where
      board_ = game ^. board
      figureItself = figure ^. figureInSelection

      tryMove :: Coord -> Action -> Maybe (Action, Game)
      tryMove movement action = do
        newCoord <- tryMoveFigure board_ figureItself coord movement
        let game' = game & state .~ PlacingFigure figure newCoord
        pure (action, game')

      placeFigureAction :: HasCallStack => IO (Maybe (Action, Game))
      placeFigureAction = do
        case tryPlaceFigure figureItself coord board_ of
          Nothing -> pure Nothing
          Just newBoard -> do
            let figuresWithSelectedPlaced = markFigureAsPlaced figure $ game ^. figures
            (newFigures, turnIncrement, nextIndices) <-
              if allPlaced $ elems figuresWithSelectedPlaced then
                fmap (, 1, [0 .. figuresToPlaceCount - 1]) $ fmap Just <$> randomFigures
              else
                pure (figuresWithSelectedPlaced, 0, nextFigureIndices $ figure ^. figureIndex)
            let game' =
                  figures .~ newFigures
                  $ board .~ removeFilledRanges newBoard
                  $ turnNumber +~ turnIncrement
                  $ game
            let maybeNextFig = tryFindNextFigureToSelect (game' ^. board) newFigures nextIndices
            pure $ case maybeNextFig of
              Just nextFig ->
                let game'' = state .~ SelectingFigure nextFig $ game' in
                Just (UserAction PlaceFigure, game'')
              Nothing ->
                let game'' = state .~ GameOver $ game' in
                Just (UserAction PlaceFigure, game'')

      actions :: HasCallStack => IO [(Action, Game)]
      actions = do
        placeAction <- placeFigureAction
        newGame <- restartGameAction
        autoPlayTurn <- nextAutoPlayTurnAction game generateAutoPlay
        pure $ catMaybes [
            tryMove vectorRight $ UserAction MoveFigureRight,
            tryMove vectorLeft $ UserAction MoveFigureLeft,
            tryMove vectorDown $ UserAction MoveFigureDown,
            tryMove vectorUp $ UserAction MoveFigureUp,
            placeAction,
            newGame,
            toggleAutoPlayAction game,
            autoPlayTurn,
            Just (UserAction CancelPlacingFigure, game & state .~ SelectingFigure figure)
          ]
    GameOver -> do
      newGame <- restartGameAction
      pure $ catMaybes [newGame]

possibleActions :: Game -> IO [(Action, Game)]
possibleActions game = possibleActionsImpl game True
