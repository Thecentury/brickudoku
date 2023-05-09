{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}

module Brickudoku
  ( Game(..),
    GameState(..),
    VersionedState(..),
    Figure,
    Cell(..),
    FigureInSelection(..),
    FigureIndex,
    Coord,
    isGameOver,
    isPlacingFigure,
    UserAction(..),
    SystemAction(..),
    Clickable(..),
    Action(..),
    GameEvent(..),
    clickableForCell,
    -- Lenses
    score,
    turnNumber,
    autoPlay,
    currentGame,
    easyMode,
    state,
    board,
    emptyFigure,
    initGame,
    figureRows,
    possibleActions,
    boardSize ) where

import Control.Lens ( (&), makeLenses, (^.), (%~), (.~), (+~), Lens' )
import Data.Array ( array, Array, listArray, elems, (//) )
import Control.Monad (replicateM, join, forM)
import Data.List (find, sort)
import Data.Maybe (mapMaybe, isNothing, catMaybes, listToMaybe, fromMaybe)
import Linear.V2 (V2(..))
import GHC.Stack (HasCallStack)
import GHC.Generics ( Generic )
import System.Random.Stateful (StatefulGen, UniformRange (uniformRM))

import MyPrelude ( mapiArray, (!), mapi, width2d, height2d )
import Undo (History(..), newHistory, put, current, tryUndoUntilDifferentL, tryRedoUntilDifferentL)
import Primitives
    ( vectorDown,
      vectorLeft,
      vectorRight,
      vectorUp,
      zeroCoord,
      Cell(..),
      Coord,
      RangeKind(..) )
import Board
    ( boardSize,
      canBePlacedToBoardAtSomePoint,
      emptyFigure,
      figuresToPlaceCount,
      pointsWhereFigureCanBePlaced,
      randomRawFigure,
      rangesToBeFreed,
      removeFilledRanges,
      tryMoveFigure,
      tryPlaceFigure,
      Board,
      Figure )

----

allPlaced :: [Maybe a] -> Bool
allPlaced = all isNothing

scoreIncrement :: [RangeKind] -> Int
scoreIncrement ranges = sum $ mapi (\i score -> (i + 1) * score) $ sort $ rangePrice <$> ranges where
  rangePrice Horizontal = 10
  rangePrice Vertical = 10
  rangePrice Square = 20

----

type FigureIndex = Int

-- todo rename, not always relates to a selection
data FigureInSelection = FigureInSelection Figure FigureIndex
  deriving stock (Show, Eq, Generic)

makeLenses ''FigureInSelection

markFigureAsPlaced :: FigureInSelection -> Array FigureIndex (Maybe FigureInSelection) -> Array FigureIndex (Maybe FigureInSelection)
markFigureAsPlaced (FigureInSelection _ ix) figures = figures // [(ix, Nothing)]

data GameState =
  SelectingFigure FigureInSelection |
  PlacingFigure FigureInSelection Coord |
  GameOver
  deriving stock (Show, Eq, Generic)

-- | Ticks mark passing of time
data GameEvent = Tick
  deriving stock (Show, Eq, Ord)

data VersionedState = VersionedState
  { _score :: !Int,
    _board :: !Board,
    _figures :: !(Array Int (Maybe FigureInSelection)),
    _state :: !GameState,
    _turnNumber :: !Int }
  deriving stock (Show, Eq, Generic)

makeLenses ''VersionedState

data Game = Game
  { _history :: !(History VersionedState),
    _autoPlay :: !Bool,
    _easyMode :: !Bool }
  deriving stock (Eq, Show, Generic)

makeLenses ''Game

currentGame :: Lens' Game VersionedState
currentGame = history . current

-- | Puts the new version of the game to the history
putNewVersion :: Game -> (VersionedState -> VersionedState) -> Game
putNewVersion g f = history %~ updateCurrent' $ g where
  updateCurrent' :: History VersionedState -> History VersionedState
  updateCurrent' = put newCurrent
  newCurrent :: VersionedState
  newCurrent = f $ g ^. currentGame

-- | Updates the current version of the game without putting it to the history
updateCurrentNotVersioned :: Game -> (VersionedState -> VersionedState) -> Game
updateCurrentNotVersioned g f = g & currentGame %~ f

isGameOver :: Game -> Bool
isGameOver game = case game ^. currentGame . state of
  GameOver -> True
  _ -> False

isPlacingFigure :: Game -> Bool
isPlacingFigure game = case game ^. currentGame . state of
  PlacingFigure _ _ -> True
  _ -> False

---

randomFigures :: (HasCallStack, StatefulGen g m) => g -> m (Array Int FigureInSelection)
randomFigures gen = do
  rawFigures <- replicateM figuresToPlaceCount (randomRawFigure gen)
  pure $ mapiArray (flip FigureInSelection) $ listArray (0, figuresToPlaceCount - 1) rawFigures

initGame :: (HasCallStack, StatefulGen g m) => g -> m Game
initGame gen = do
  let _board =
        array
          (V2 0 0, V2 (boardSize - 1) (boardSize - 1))
          [(V2 x y, Free) | x <- [0 .. boardSize - 1], y <- [0 .. boardSize - 1]]
  boardFigures <- randomFigures gen
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

rowCells :: HasCallStack => Int -> Array Coord a -> [(a, Clickable)]
rowCells rowIndex f =
  [(f ! V2 x rowIndex, clickableForCell $ V2 x rowIndex) | x <- [0 .. figureWidth - 1]]
    where
      figureWidth = width2d f

figureRows :: HasCallStack => Array Coord a -> [[(a, Clickable)]]
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

data Clickable =
  PlaceFigureClickable Coord |
  SelectFigureClickable FigureIndex
  deriving stock (Eq, Show, Ord)

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
  SystemAction SystemAction |
  Click Clickable
  deriving stock (Eq, Show)

onlyUserAction :: Action -> Maybe UserAction
onlyUserAction (UserAction a) = Just a
onlyUserAction _ = Nothing

userActionProbability :: UserAction -> Int
userActionProbability PlaceFigure = 30
userActionProbability CancelPlacingFigure = 1
userActionProbability _ = 10

restartGameAction :: StatefulGen g m => g -> m (Maybe (Action, Game))
restartGameAction gen = (\g -> Just (SystemAction RestartGame, g)) <$> initGame gen

toggleAutoPlayAction :: Game -> Maybe (Action, Game)
toggleAutoPlayAction game =
  Just (SystemAction ToggleAutoPlay, game & autoPlay %~ not)

nextAutoPlayTurnAction :: StatefulGen g m => g -> Game -> Bool -> m (Maybe (Action, Game))
nextAutoPlayTurnAction gen game generateAutoPlay = do
  if game ^. autoPlay && generateAutoPlay then
    nextAction gen
  else
    pure Nothing
  where
    nextAction :: StatefulGen g m => g -> m (Maybe (Action, Game))
    nextAction gen' = do
      actions <- possibleActionsImpl gen' game False
      let applicableActions = mapMaybe (\(a, g) -> (, g) <$> onlyUserAction a) actions
      if null applicableActions then
        pure Nothing
      else do
        (_, game') <- randomElement gen' $ actionsAccordingToProbability applicableActions
        pure $ Just (SystemAction NextAutoPlayTurn, game')

actionsAccordingToProbability :: [(UserAction, a)] -> [(UserAction, a)]
actionsAccordingToProbability = concatMap (\(action, game) -> replicate (userActionProbability action) (action, game))

randomElement :: StatefulGen g m => g -> [a] -> m a
randomElement gen list = do
  randomIndex <- uniformRM (0, length list - 1) gen
  pure $ list !! randomIndex

clickableFigureAction :: Game -> FigureIndex -> Maybe (Action, Game)
clickableFigureAction game figureIx = do
  fig <- (game ^. currentGame . figures) ! figureIx
  let (FigureInSelection figureItself _) = fig
  let coord = fromMaybe zeroCoord $ listToMaybe $ pointsWhereFigureCanBePlaced figureItself (game ^. currentGame . board)
  let game' = updateCurrentNotVersioned game $ state .~ PlacingFigure fig coord
  pure (Click $ SelectFigureClickable figureIx, game')

placeFigureAction :: StatefulGen g m => g -> Game -> FigureInSelection -> Action -> Coord -> m (Maybe (Action, Game))
placeFigureAction gen' game figure@(FigureInSelection selectedFigure figureIndex) action coord = do
  case tryPlaceFigure selectedFigure coord (game ^. currentGame . board) of
    Nothing -> pure Nothing
    Just newBoard -> do
      let figuresWithSelectedPlaced = markFigureAsPlaced figure $ game ^. currentGame . figures
      (newFigures, turnIncrement, nextIndices) <-
        if allPlaced $ elems figuresWithSelectedPlaced then
          fmap (, 1, [0 .. figuresToPlaceCount - 1]) $ fmap Just <$> randomFigures gen'
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
          Just (action, game')
        Nothing ->
          let game' = putNewVersion game $ const $ state .~ GameOver $ state' in
          Just (action, game')

clickToPlaceFigureActions :: StatefulGen g m => g -> Game -> FigureInSelection -> m [(Action, Game)]
clickToPlaceFigureActions gen game figure@(FigureInSelection selectedFigure _) = do
  let startCoordinates = pointsWhereFigureCanBePlaced selectedFigure (game ^. currentGame . board)
  fmap catMaybes <$> forM startCoordinates $ \coord -> placeFigureAction gen game figure (Click $ PlaceFigureClickable coord) coord

-- todo create own monad/type like Tetris does?
-- todo merge with 'possibleActions'?
possibleActionsImpl :: StatefulGen g m => g -> Game -> Bool -> m [(Action, Game)]
possibleActionsImpl gen game generateAutoPlay = do
  case game ^. currentGame . state of
    SelectingFigure figure@(FigureInSelection selectedFigure figureIndex) -> actions where
      actions = do
        newGame <- restartGameAction gen
        autoPlayTurn <- nextAutoPlayTurnAction gen game generateAutoPlay
        clickToPlaceActions <- clickToPlaceFigureActions gen game figure
        pure $ clickToPlaceActions ++ catMaybes
          [
            moveFigure (UserAction SelectNextFigure) nextFigureIndices,
            moveFigure (UserAction SelectPreviousFigure) previousFigureIndices,
            clickableFigureAction game $ nextFigureIndices figureIndex !! 0,
            clickableFigureAction game $ nextFigureIndices figureIndex !! 1,
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

    PlacingFigure figure@(FigureInSelection selectedFigure figureIndex) coord -> actions gen where
      board_ = game ^. currentGame . board

      tryMove :: Coord -> Action -> Maybe (Action, Game)
      tryMove movement action = do
        newCoord <- tryMoveFigure board_ selectedFigure coord movement
        let game' = updateCurrentNotVersioned game $ state .~ PlacingFigure figure newCoord
        pure (action, game')

      actions :: StatefulGen g m => g -> m [(Action, Game)]
      actions gen' = do
        placeAction <- placeFigureAction gen' game figure (UserAction PlaceFigure) coord
        newGame <- restartGameAction gen'
        autoPlayTurn <- nextAutoPlayTurnAction gen' game generateAutoPlay
        clickToPlaceActions <- clickToPlaceFigureActions gen' game figure
        pure $ clickToPlaceActions ++ catMaybes [
            tryMove vectorRight $ UserAction MoveFigureRight,
            tryMove vectorLeft $ UserAction MoveFigureLeft,
            tryMove vectorDown $ UserAction MoveFigureDown,
            tryMove vectorUp $ UserAction MoveFigureUp,
            clickableFigureAction game $ nextFigureIndices figureIndex !! 0,
            clickableFigureAction game $ nextFigureIndices figureIndex !! 1,
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
      newGame <- restartGameAction gen
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

possibleActions :: StatefulGen g m => Game -> g -> m [(Action, Game)]
possibleActions game gen = possibleActionsImpl gen game True

-------------------------------------------------------------------------------

clickableForCell :: Coord -> Clickable
clickableForCell = PlaceFigureClickable
