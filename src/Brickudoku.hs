{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Brickudoku
  ( Game,
    GameState(..),
    Figure,
    FreeStyle(..),
    VisualCell(..),
    Cell(..),
    FigureInSelection(..),
    FigureToPlace(..),
    FigureToPlaceKind(..),
    HintPlacementResult(..),
    Coord,
    cellsToDisplay,
    isGameOver,
    isPlacingFigure,
    UserAction(..),
    SystemAction(..),
    Action(..),
    GameEvent(..),
    figuresToPlace,
    -- Lenses
    score,
    turnNumber,
    autoPlay,
    currentGame,
    emptyFigure,
    initGame,
    figureRows,
    possibleActions,
    boardSize ) where

import Control.Lens ( (&), makeLenses, (^.), (%~), (.~), (+~), Lens' )
import Data.Array ( (//), array, bounds, Array, assocs, listArray, elems )
import System.Random.Stateful ( globalStdGen, UniformRange(uniformRM) )
import Control.Monad (replicateM, join)
import Data.List (find, sort, sortOn)
import Data.Maybe (mapMaybe, isNothing, catMaybes, listToMaybe, fromMaybe)
import Linear.V2 (V2(..), _x, _y)
import MyPrelude ( mapiArray, (!), mapi, width2d, height2d )
import qualified Data.Bifunctor
import System.Random (randomRIO)
import GHC.Stack (HasCallStack)
import Undo (History(..), newHistory, put, current, tryUndoUntilDifferentL, tryRedoUntilDifferentL)
import Data.List.Extra (groupOnKey)

----

data Cell =
  Free | Filled
  deriving stock (Show, Eq)

data FreeStyle = PrimaryStyle | AltStyle
  deriving stock (Show, Eq, Ord)

data HintPlacementResult = JustFigure | Region
  deriving stock (Show, Eq, Ord)

data VisualCell =
  VFree FreeStyle |
  VFilled |
  VWillBeFreed |
  VCanPlaceFullFigure |
  VCanPlaceButNotFullFigure |
  VCannotPlace |
  VCanBePlacedHint FreeStyle HintPlacementResult
  deriving stock (Show, Eq, Ord)

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

type Figure = Array Coord Cell

figureCellCoords :: Figure -> [Coord]
figureCellCoords = fmap fst . filter (\(_, e) -> e == Filled) . assocs

type Board = Figure

type PlacingCellsFigure = Array Coord VisualCell

tryMoveFigure :: Board -> Figure -> Coord -> Coord -> Maybe Coord
tryMoveFigure board figure coord vector =
  let
    newCoord = coord + vector
    figureSize = snd $ bounds figure
    (boardTopLeft, boardBottomRight) = bounds board
    topLeftWithinBoard =
      newCoord ^. _x >= boardTopLeft ^. _x &&
      newCoord ^. _y >= boardTopLeft ^. _y
    bottomRightWithinBoard =
      newCoord ^. _x + figureSize ^. _x <= boardBottomRight ^. _x &&
      newCoord ^. _y + figureSize ^. _y <= boardBottomRight ^. _y
  in
    if topLeftWithinBoard && bottomRightWithinBoard then
      Just newCoord
    else
      Nothing

----

boardSize, figuresToPlaceCount :: Int
boardSize = 9
figuresToPlaceCount = 3

-- | Frees cells by specified coordinates
freeAllCells :: Board -> [Coord] -> Board
freeAllCells b coords =
  b // fmap (, Free) coords

-- | Determines whether all cells by specified coordinates are filled
allCellsAreFilled :: HasCallStack => Figure -> [Coord] -> Bool
allCellsAreFilled fig coords =
  coords & fmap (fig !) & all (== Filled)

data RangeKind =
  Horizontal |
  Vertical |
  Square
  deriving stock (Show, Eq)

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

scoreIncrement :: HasCallStack => [RangeKind] -> Int
scoreIncrement ranges = sum $ mapi (\i score -> (i + 1) * score) $ sort $ rangePrice <$> ranges where
  rangePrice Horizontal = 10
  rangePrice Vertical = 10
  rangePrice Square = 20

removeFilledRanges :: HasCallStack => Board -> Board
removeFilledRanges b = foldl (\b' (_, range) -> freeAllCells b' range) b $ rangesToBeFreed b

possibleFigureStartCoordinates :: HasCallStack => Board -> [Coord]
possibleFigureStartCoordinates b =
  [V2 x y | y <- [0 .. boardSize - figureHeight - 1], x <- [0 .. boardSize - figureWidth - 1]] where
    -- todo are width and height here correct?
    (V2 figureHeight figureWidth) = snd $ bounds b

pointsWhereFigureCanBePlaced :: HasCallStack => Figure -> Board -> [Coord]
pointsWhereFigureCanBePlaced fig b =
  mapMaybe (\coord -> coord <$ tryPlaceFigure fig coord b) $ possibleFigureStartCoordinates fig

canBePlacedToBoardAtSomePoint :: HasCallStack => Figure -> Board -> Bool
canBePlacedToBoardAtSomePoint fig b =
  not . null $ pointsWhereFigureCanBePlaced fig b

----

boardCellToPlacingCell :: HasCallStack => Cell -> VisualCell
boardCellToPlacingCell Free = VFree PrimaryStyle
boardCellToPlacingCell Filled = VFilled

boardToPlacingCells :: HasCallStack => Board -> Array Coord VisualCell
boardToPlacingCells board =
  board
  & assocs
  & map (Data.Bifunctor.second boardCellToPlacingCell)
  & array (bounds board)

addAltStyleCells :: HasCallStack => Array Coord VisualCell -> Array Coord VisualCell
addAltStyleCells cells = cells // mapMaybe addAltStyleCell (assocs cells) where
  addAltStyleCell :: (Coord, VisualCell) -> Maybe (Coord, VisualCell)
  addAltStyleCell (coord, VFree PrimaryStyle) =
    if isAltStyleCell coord then
      Just (coord, VFree AltStyle)
    else
      Nothing
  addAltStyleCell _ = Nothing

  isAltStyleCell :: Coord -> Bool
  isAltStyleCell (V2 x y) = isAltStyleCellThick (thickColumnNumber x) (thickColumnNumber y)

  isAltStyleCellThick 0 1 = True 
  isAltStyleCellThick 1 0 = True 
  isAltStyleCellThick 1 2 = True 
  isAltStyleCellThick 2 1 = True
  isAltStyleCellThick _ _ = False 

  thickColumnNumber :: Int -> Int
  thickColumnNumber x = x `div` 3

markFigureAsPlaced :: HasCallStack => FigureInSelection -> Array FigureIndex (Maybe FigureInSelection) -> Array FigureIndex (Maybe FigureInSelection)
markFigureAsPlaced (FigureInSelection _ ix) figures = figures // [(ix, Nothing)]

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

addPlacingFigure :: HasCallStack => Figure -> Coord -> Board -> PlacingCellsFigure
addPlacingFigure figure figureCoord board =
  placingBoard // toBeFreedCells // figureCells
  where
    placingBoard = boardToPlacingCells board
    boardWithFigure = fromMaybe board $ tryPlaceFigure figure figureCoord board
    rangesWillBeFreed = snd =<< rangesToBeFreed boardWithFigure

    figureCells =
      figure
      & assocs
      & filter (\(_, cell) -> cell == Filled)
      & map (\(coord, cell) -> (newCoord coord, figureCell (newCoord coord) cell))

    toBeFreedCells = (, VWillBeFreed) <$> rangesWillBeFreed

    newCoord :: Coord -> Coord
    newCoord c = c + figureCoord

    canPlaceFullFigure = all (\coord -> board ! coord == Free) $ newCoord <$> figureCellCoords figure

    figureCell :: HasCallStack => Coord -> Cell -> VisualCell
    figureCell boardCoord figCell =
      case (board ! boardCoord, figCell, canPlaceFullFigure) of
        (Filled, Filled, _)     -> VCannotPlace
        (Filled, Free,   _)     -> VFilled
        (Free,   Filled, True)  -> VCanPlaceFullFigure
        (Free,   Filled, False) -> VCanPlaceButNotFullFigure
        (Free,   Free,   _)     -> VFree PrimaryStyle

type FigureIndex = Int

-- todo rename, not always relates to a selection
data FigureInSelection = FigureInSelection Figure FigureIndex
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

data VersionedState = VersionedState
  { _score :: Int,
    _board :: Board,
    _figures :: Array Int (Maybe FigureInSelection),
    _state :: GameState,
    _turnNumber :: Int }
  deriving stock (Show, Eq)

makeLenses ''VersionedState

data Game = Game
  { _history :: History VersionedState,
    _autoPlay :: Bool,
    _easyMode :: Bool }
  deriving stock (Show)

makeLenses ''Game

currentGame :: Lens' Game VersionedState
currentGame = history . current

-- | Puts the new version of the game to the history
putNewVersion :: HasCallStack => Game -> (VersionedState -> VersionedState) -> Game
putNewVersion g f = history %~ updateCurrent' $ g where
  updateCurrent' :: History VersionedState -> History VersionedState
  updateCurrent' = put newCurrent
  newCurrent :: VersionedState
  newCurrent = f $ g ^. currentGame

-- | Updates the current version of the game without putting it to the history
updateCurrentNotVersioned :: HasCallStack => Game -> (VersionedState -> VersionedState) -> Game
updateCurrentNotVersioned g f = g & currentGame %~ f

isGameOver :: Game -> Bool
isGameOver game = case game ^. currentGame . state of
  GameOver -> True
  _ -> False

isPlacingFigure :: Game -> Bool
isPlacingFigure game = case game ^. currentGame . state of
  PlacingFigure _ _ -> True
  _ -> False

mergeCellHelpingHighlight :: HasCallStack => VisualCell -> VisualCell -> VisualCell
mergeCellHelpingHighlight (VFree style) (VCanBePlacedHint _ result) = VCanBePlacedHint style result
mergeCellHelpingHighlight existing _ = existing

addHelpHighlightForFigure :: HasCallStack => HasCallStack => Board -> Figure -> Array Coord VisualCell -> Array Coord VisualCell
addHelpHighlightForFigure b fig cells =
  cells // mergedCellsToUpdate where
    figureCoords = figureCellCoords fig
    canBePlaced = (\startPos -> (startPos, (+ startPos) <$> figureCoords)) <$> pointsWhereFigureCanBePlaced fig b
    currentCells = 
      concatMap (\(startCoord, coords) -> 
        (\coord -> (startCoord, coord, cells ! coord)) <$> coords) 
        canBePlaced
    -- Here single coordinate can occur multiple times, need to merge the cells
    cellsToUpdate =
      (\(startCoord, coord, boardCell) -> (coord, mergeCellHelpingHighlight boardCell $ VCanBePlacedHint PrimaryStyle (placementResult startCoord))) 
      <$> currentCells
    mergedCellsToUpdate =
      -- Drop the grouping key (coord)
      map snd
      -- Here we use that 'JustFigure' < 'Region'. For each list of cells with the same coord use max of them.
      $ Data.Bifunctor.second maximum
      -- Convert to [(Coord, [(Coord, VisualCell)])]
      <$> groupOnKey fst (sortOn fst cellsToUpdate)
    placementResult :: HasCallStack => Coord -> HintPlacementResult
    placementResult coord =
      case tryPlaceFigure fig coord b of
        Just newBoard ->
          if null $ rangesToBeFreed newBoard then
            JustFigure
          else
            Region
        Nothing -> JustFigure

addHelpHighlight :: HasCallStack => Game -> Array Coord VisualCell -> Array Coord VisualCell
addHelpHighlight g cells | not (g ^. easyMode) = cells
addHelpHighlight (Game (History (VersionedState _ _ _ GameOver _) _ _) _ _) cells = cells
addHelpHighlight (Game (History (VersionedState _ b _ (SelectingFigure (FigureInSelection fig _)) _) _ _) _ _) cells =
  addHelpHighlightForFigure b fig cells
addHelpHighlight (Game (History (VersionedState _ b _ (PlacingFigure (FigureInSelection fig _) _) _) _ _) _ _) cells =
  addHelpHighlightForFigure b fig cells

cellsToDisplay :: HasCallStack => Game -> PlacingCellsFigure
cellsToDisplay game = case game ^. currentGame . state of
  PlacingFigure (FigureInSelection figure _) coord -> 
    wrap $ addPlacingFigure figure coord brd
  _ ->
    wrap $ boardToPlacingCells brd
  where
    wrap cells = addHelpHighlight game $ addAltStyleCells cells
    brd = game ^. currentGame . board
      
figuresToPlace :: HasCallStack => Game -> [Maybe (FigureToPlace FigureInSelection)]
figuresToPlace game =
  game ^. currentGame 
  & _figures
  & elems
  & map (fmap (\fig -> FigureToPlace fig $ kind fig)) where
    kind :: FigureInSelection -> FigureToPlaceKind
    kind fig =
      case game ^. currentGame . state of
        GameOver -> CannotBePlaced
        SelectingFigure selectedFigure -> canBePlaced selectedFigure fig Selected
        PlacingFigure selectedFigure _ -> canBePlaced selectedFigure fig SelectedPlacing
    canBePlaced :: FigureInSelection -> FigureInSelection -> FigureToPlaceKind -> FigureToPlaceKind
    canBePlaced selectedFigure fig@(FigureInSelection figureItself _) selectedMode
      | fig == selectedFigure = selectedMode
      | canBePlacedToBoardAtSomePoint figureItself (game ^. currentGame . board) = CanBePlaced
      | otherwise = CannotBePlaced      

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

mkFigure :: HasCallStack => [[Int]] -> Figure
mkFigure idx =
  array (V2 0 0, V2 (figureWidth - 1) (figureHeight - 1)) [(V2 x y, intToCell $ numberAt (V2 x y)) | y <- [0 .. figureHeight - 1], x <- [0 .. figureWidth - 1]]
  where
    figureHeight = length idx
    figureWidth = length (head idx)

    numberAt :: Coord -> Int
    numberAt (V2 x y) = (idx !! y) !! x

    intToCell :: Int -> Cell
    intToCell 0 = Free
    intToCell _ = Filled

possibleFigures :: HasCallStack => [Figure]
possibleFigures = map mkFigure possibleFiguresData

emptyFigure :: Figure
emptyFigure = mkFigure [[0]]

rotateFigureClockwise :: HasCallStack => Figure -> Figure
rotateFigureClockwise f =
  array (V2 0 0, V2 (newWidth - 1) (newHeight - 1)) [(V2 y (figureHeight - 1 - y), f ! V2 x y) | y <- [0 .. figureHeight - 1], x <- [0 .. figureWidth - 1]]
    where
      figureWidth = width2d f
      figureHeight = height2d f
      newWidth = figureHeight
      newHeight = figureWidth

randomRawFigure :: HasCallStack => IO Figure
randomRawFigure = do
  ix <- uniformRM (0, length possibleFigures - 1) globalStdGen
  rotations <- uniformRM (0 :: Int, 3) globalStdGen
  let figure = possibleFigures !! ix
  return $ iterate rotateFigureClockwise figure !! rotations

---

randomFigures :: HasCallStack => IO (Array Int FigureInSelection)
randomFigures = do
  rawFigures <- replicateM figuresToPlaceCount randomRawFigure
  pure $ mapiArray (flip FigureInSelection) $ listArray (0, figuresToPlaceCount - 1) rawFigures

initGame :: HasCallStack => IO Game
initGame = do
  putStrLn "Before _board"
  let _board =
        array 
          (V2 0 0, V2 (boardSize - 1) (boardSize - 1))
          [(V2 x y, Free) | x <- [0 .. boardSize - 1], y <- [0 .. boardSize - 1]]
  putStrLn "Before randomFigures"
  boardFigures <- randomFigures
  let justFigures = Just <$> boardFigures
  let coreGame = VersionedState
        { _score = 0,
          _board = _board,
          _figures = justFigures,
          _state = SelectingFigure $ boardFigures ! 0,
          _turnNumber = 1 }
  let game = Game 
        { _history = newHistory coreGame,
          _autoPlay = False,
          _easyMode = False }
  return game

rowCells :: HasCallStack => Int -> Array Coord a -> [a]
rowCells rowIndex f =
  [f ! V2 x rowIndex | x <- [0 .. figureWidth - 1]]
    where
      figureWidth = width2d f

figureRows :: HasCallStack => Array Coord a -> [[a]]
figureRows f = (`rowCells` f) <$> rowIndices where
  figureHeight = height2d f
  rowIndices = [0 .. figureHeight - 1]

--- Event handling ---

nextFigureIndices :: FigureIndex -> [FigureIndex]
nextFigureIndices currentFigureIndex =
  map (`rem` figuresToPlaceCount) [currentFigureIndex + 1, currentFigureIndex + 2]

previousFigureIndices :: FigureIndex -> [FigureIndex]
previousFigureIndices currentFigureIndex =
  (`rem` figuresToPlaceCount) <$> [figuresToPlaceCount + currentFigureIndex - 1, figuresToPlaceCount + currentFigureIndex - 2]

tryFindNextFigureToSelect :: HasCallStack => Figure -> Array FigureIndex (Maybe FigureInSelection) -> [FigureIndex] -> Maybe FigureInSelection
tryFindNextFigureToSelect b figs nextIndices =
  join $ find canBeSelected nexts where
    canBeSelected :: Maybe FigureInSelection -> Bool
    canBeSelected Nothing = False
    canBeSelected (Just (FigureInSelection f _)) = canBePlacedToBoardAtSomePoint f b

    nexts :: [Maybe FigureInSelection]
    nexts = (figs !) <$> nextIndices

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
  ToggleAutoPlay |
  Undo |
  Redo |
  ToggleEasyMode
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
  case game ^. currentGame . state of
    SelectingFigure figure@(FigureInSelection selectedFigure figureIndex) -> actions where
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
            toggleEasyModeAction game,
            autoPlayTurn,
            undoAction game,
            redoAction game
          ]

      moveFigure :: HasCallStack => Action -> (FigureIndex -> [FigureIndex]) -> Maybe (Action, Game)
      moveFigure action calculateNextIndices = do
        let nextIndices = calculateNextIndices figureIndex
        nextFigure <- tryFindNextFigureToSelect (game ^. currentGame . board) (game ^. currentGame . figures) nextIndices
        let game' = updateCurrentNotVersioned game $ state .~ SelectingFigure nextFigure
        pure (action, game')

      startPlacing = do
        let coord = fromMaybe zeroCoord $ listToMaybe $ pointsWhereFigureCanBePlaced selectedFigure (game ^. currentGame . board)
        let game' = updateCurrentNotVersioned game $ state .~ PlacingFigure figure coord
        pure (UserAction StartPlacingFigure, game')

    PlacingFigure figure@(FigureInSelection selectedFigure figureIndex) coord -> actions where
      board_ = game ^. currentGame . board

      tryMove :: HasCallStack => Coord -> Action -> Maybe (Action, Game)
      tryMove movement action = do
        newCoord <- tryMoveFigure board_ selectedFigure coord movement
        let game' = updateCurrentNotVersioned game $ state .~ PlacingFigure figure newCoord
        pure (action, game')

      placeFigureAction :: HasCallStack => IO (Maybe (Action, Game))
      placeFigureAction = do
        case tryPlaceFigure selectedFigure coord board_ of
          Nothing -> pure Nothing
          Just newBoard -> do
            let figuresWithSelectedPlaced = markFigureAsPlaced figure $ game ^. currentGame . figures
            (newFigures, turnIncrement, nextIndices) <-
              if allPlaced $ elems figuresWithSelectedPlaced then
                fmap (, 1, [0 .. figuresToPlaceCount - 1]) $ fmap Just <$> randomFigures
              else
                pure (figuresWithSelectedPlaced, 0, nextFigureIndices figureIndex)
            let scoreIncr = scoreIncrement $ fst <$> rangesToBeFreed newBoard
            let state' =
                  figures .~ newFigures
                  $ board .~ removeFilledRanges newBoard
                  $ turnNumber +~ turnIncrement
                  $ score +~ scoreIncr
                  $ game ^. currentGame
            let maybeNextFig = tryFindNextFigureToSelect (state' ^. board) newFigures nextIndices
            pure $ case maybeNextFig of
              Just nextFig ->
                let
                  state'' = state .~ SelectingFigure nextFig $ state'
                  game' = putNewVersion game $ const state''
                in
                Just (UserAction PlaceFigure, game')
              Nothing ->
                let game' = putNewVersion game $ const $ state .~ GameOver $ state' in
                Just (UserAction PlaceFigure, game')

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
            toggleEasyModeAction game,
            autoPlayTurn,
            undoAction game,
            redoAction game,
            Just (UserAction CancelPlacingFigure, updateCurrentNotVersioned game $ state .~ SelectingFigure figure)
          ]
    GameOver -> do
      newGame <- restartGameAction
      pure $ catMaybes [
          newGame,
          undoAction game,
          toggleAutoPlayAction game,
          toggleEasyModeAction game
        ]

undoAction :: Game -> Maybe (Action, Game)
undoAction game =
  case game ^. history & tryUndoUntilDifferentL board of
    Nothing -> Nothing
    Just history' -> Just (SystemAction Undo, game & history .~ history')

redoAction :: Game -> Maybe (Action, Game)
redoAction game =
  case game ^. history & tryRedoUntilDifferentL board of
    Nothing -> Nothing
    Just history' -> Just (SystemAction Redo, game & history .~ history')

toggleEasyModeAction :: Game -> Maybe (Action, Game)
toggleEasyModeAction game =
  Just (SystemAction ToggleEasyMode, game') where
    game' = game & easyMode %~ not 

possibleActions :: Game -> IO [(Action, Game)]
possibleActions game = possibleActionsImpl game True
