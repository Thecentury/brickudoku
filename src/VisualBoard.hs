{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TupleSections #-}

module VisualBoard
  (
    cellsToDisplay,
    figuresToPlace,
    FreeStyle(..),
    VisualCell(..),
    FigureToPlace(..),
    FigureToPlaceKind(..),
    HintPlacementResult(..)
    )
  where

import Data.Array ( (//), array, bounds, Array, assocs )
import qualified Data.Bifunctor
import Linear.V2 (V2(..))

import Primitives
    ( Cell(..),
      Coord )
import Board
    ( figureCellCoords,
      rangesToBeFreed,
      tryPlaceFigure,
      Board,
      Figure, pointsWhereFigureCanBePlaced, canBePlacedToBoardAtSomePoint )
import Brickudoku
 (
    FigureInSelection(..),
    Game(..),
    GameState (..),
    VersionedState (..),
    easyMode,
    currentGame,
    state,
    board, Clickable (..) )
import GHC.Stack (HasCallStack)
import Data.Function ((&))
import Data.Maybe (mapMaybe, fromMaybe)
import MyPrelude ((!))
import Undo (History(..))
import Control.Lens ((^.))
import Data.List.Extra (groupOnKey)
import Data.List (sortOn)

--------------------------------------------------------------------------------

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

boardCellToPlacingCell :: Cell -> VisualCell
boardCellToPlacingCell Free = VFree PrimaryStyle
boardCellToPlacingCell Filled = VFilled

boardToPlacingCells :: Board -> Array Coord VisualCell
boardToPlacingCells brd =
  brd
  & assocs
  & map (Data.Bifunctor.second boardCellToPlacingCell)
  & array (bounds brd)

addAltStyleCells :: Array Coord VisualCell -> Array Coord VisualCell
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

type PlacingCellsFigure = Array Coord VisualCell

addPlacingFigure :: HasCallStack => Figure -> Coord -> Board -> PlacingCellsFigure
addPlacingFigure figure figureCoord brd =
  placingBoard // toBeFreedCells // figureCells
  where
    placingBoard = boardToPlacingCells brd
    boardWithFigure = fromMaybe brd $ tryPlaceFigure figure figureCoord brd
    rangesWillBeFreed = snd =<< rangesToBeFreed boardWithFigure

    figureCells =
      figure
      & assocs
      & filter (\(_, cell) -> cell == Filled)
      & map (\(coord, cell) -> (newCoord coord, figureCell (newCoord coord) cell))

    toBeFreedCells = (, VWillBeFreed) <$> rangesWillBeFreed

    newCoord :: Coord -> Coord
    newCoord c = c + figureCoord

    canPlaceFullFigure = all (\coord -> brd ! coord == Free) $ newCoord <$> figureCellCoords figure

    figureCell :: HasCallStack => Coord -> Cell -> VisualCell
    figureCell boardCoord figCell =
      case (brd ! boardCoord, figCell, canPlaceFullFigure) of
        (Filled, Filled, _)     -> VCannotPlace
        (Filled, Free,   _)     -> VFilled
        (Free,   Filled, True)  -> VCanPlaceFullFigure
        (Free,   Filled, False) -> VCanPlaceButNotFullFigure
        (Free,   Free,   _)     -> VFree PrimaryStyle

mergeCellHelpingHighlight :: VisualCell -> VisualCell -> VisualCell
mergeCellHelpingHighlight (VFree style) (VCanBePlacedHint _ result) = VCanBePlacedHint style result
mergeCellHelpingHighlight existing _ = existing

addHelpHighlightForFigure :: HasCallStack => Board -> Figure -> Array Coord VisualCell -> Array Coord VisualCell
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

--------------------------------------------------------------------------------

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

figuresToPlace :: HasCallStack => Game -> [Maybe (FigureToPlace FigureInSelection, Clickable)]
figuresToPlace game =
  game ^. currentGame
  & _figures
  & assocs
  & map (\(ix, maybeFigure) -> fmap (\fig -> (FigureToPlace fig $ kind fig, SelectFigureClickable ix)) maybeFigure) where
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