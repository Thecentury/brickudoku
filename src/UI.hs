{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module UI (main) where

import Blockudoku
    ( Game,
      State(PlacingFigure, SelectingFigure),
      Figure,
      CellCoord,
      Selectable(..),
      PlacingCell(..),
      Cell(..),
      boardToPlacingCells,
      addPlacingFigure,
      Action(..),
      board,
      figures,
      score,
      state,
      turnNumber,
      emptyFigure,
      initGame,
      figureRows,
      possibleActions )

import Control.Monad.State.Strict
    ( MonadIO(liftIO), MonadState(put, get) )
import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Widget
  , customMain, neverShowCursor
  , halt
  , hLimit, vBox, hBox, padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle, str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>), (<=>), attrName, joinBorders, padLeftRight, vLimit, updateAttrMap)
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Lens ((^.), (&))
import qualified Graphics.Vty as V
import Data.Array (elems, Array)
import Data.Functor (void)
import GHC.Conc.Sync (getUncaughtExceptionHandler, setUncaughtExceptionHandler)
import Control.Exception (SomeException, Exception (displayException), handle)
import Data.Map (Map)
import qualified Data.Map as Map

type Name = ()

app :: App Game () Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return ()
          , appAttrMap = const theMap
          }

keyBindings :: Map (V.Key, [V.Modifier]) [Action]
keyBindings =
  Map.fromList [
    ((V.KRight, []), [MoveFigureRight, SelectNextFigure]),
    ((V.KLeft, []), [MoveFigureLeft, SelectPreviousFigure]),
    ((V.KUp, []), [MoveFigureUp]),
    ((V.KDown, []), [MoveFigureDown]),
    ((V.KEnter, []), [StartPlacingFigure, PlaceFigure]),
    ((V.KEsc, []), [CancelPlacingFigure]),
    ((V.KChar 'R', []), [RestartGame])
  ]

handleEvent :: BrickEvent Name () -> EventM Name Game ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'Q') [])) = halt
handleEvent (VtyEvent (V.EvKey key modifiers)) = do
  game <- get
  actions <- liftIO $ possibleActions game
  case Map.lookup (key, modifiers) keyBindings of
    Just applicableKeyBindingActions ->
      let
        actionsToApply = filter (\(a, _) -> a `elem` applicableKeyBindingActions) actions
      in
        case actionsToApply of
          [] -> pure ()
          [(_, newGame)] -> put newGame
          _ -> error $ "Multiple applicable actions for key " ++ show key ++ " " ++ show modifiers ++ ": " ++ show (fmap fst actionsToApply)
    Nothing -> pure ()
handleEvent _ = pure ()

drawUI :: Game -> [Widget Name]
drawUI game =
  [
    C.center (centralColumn <+> padLeft (Pad 2) (drawScore game))
  ] where
    centralColumn = C.hCenter (str $ "Turn: " ++ show (game ^. turnNumber)) <=> C.hCenter (drawGrid game) <=> padTop (Pad 1) figuresToPlace
    figuresToPlace =
      C.hCenter
      $ withBorderStyle BS.unicodeRounded
      $ B.border
      $ hBox
      $ map (vLimit 6 . C.vCenter . drawFigureToPlace (game ^. Blockudoku.state == SelectingFigure)) $ elems $ game ^. figures

drawScore :: Game -> Widget Name
drawScore game =
  hLimit 9
  $ withBorderStyle BS.unicodeRounded
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show $ game ^. score

drawGrid :: Game -> Widget Name
drawGrid game =
  withBorderStyle BS.unicodeRounded
  $ B.border
  $ padAll 0
  $ drawFigure drawPlacingCell boardToDraw
  where
    boardToDraw = case game ^. Blockudoku.state of
      PlacingFigure figure coord -> addPlacingFigure figure coord $ game ^. board
      _ -> game ^. board & boardToPlacingCells

filledCell :: Widget n
filledCell = str "  "
emptyCell :: Widget n
emptyCell  = str "· "

drawPlacingCell :: PlacingCell -> Widget Name
drawPlacingCell PlacingFree = withAttr emptyCellAttr emptyCell
drawPlacingCell PlacingFilled = withAttr filledCellAttr filledCell
drawPlacingCell PlacingCanPlaceFullFigure = withAttr placingCanPlaceFullFigureAttr filledCell
drawPlacingCell PlacingCanPlaceButNotFullFigure = withAttr placingCanPlaceButNotFullFigure filledCell
drawPlacingCell PlacingCannotPlace = withAttr placingCannotPlaceAttr filledCell

drawFigure :: (a -> Widget Name) -> Array CellCoord a -> Widget Name
drawFigure drawOneCell figure = vBox cellRows where
  cellRows = map (hBox . map drawOneCell) $ figureRows figure

selectedFigureBorderMappings :: [(A.AttrName, V.Attr)]
selectedFigureBorderMappings =
    [ (B.borderAttr, fg V.yellow) ]

selectedPlacingFigureBorderMappings :: [(A.AttrName, V.Attr)]
selectedPlacingFigureBorderMappings =
    [ (B.borderAttr, fg V.brightYellow) ]

notSelectedFigureBorderMappings :: [(A.AttrName, V.Attr)]
notSelectedFigureBorderMappings =
    [ (B.borderAttr, fg V.white) ]

drawSomeFigureToPlace :: [(A.AttrName, V.Attr)] -> BS.BorderStyle -> (Cell -> Widget Name) -> Figure -> Widget Name
drawSomeFigureToPlace mapping borderStyle drawOneCell figure =
 padLeftRight 2
 $ updateAttrMap (A.applyAttrMappings mapping)
 $ withBorderStyle borderStyle
 $ B.border
 $ hLimit 10
 $ vLimit 6
 $ C.center
 $ drawFigure drawOneCell figure

drawFigureToPlace :: Bool -> Maybe (Selectable Figure) -> Widget Name
drawFigureToPlace _ Nothing = drawSomeFigureToPlace notSelectedFigureBorderMappings BS.unicodeRounded (\_ -> withAttr emptyCellAttr $ str "  ") emptyFigure
drawFigureToPlace True (Just (Selected figure)) = drawSomeFigureToPlace selectedFigureBorderMappings BS.unicodeBold drawCell figure
drawFigureToPlace False (Just (Selected figure)) = drawSomeFigureToPlace selectedPlacingFigureBorderMappings BS.unicodeRounded drawCell figure
drawFigureToPlace _ (Just (NotSelected figure)) = drawSomeFigureToPlace notSelectedFigureBorderMappings BS.unicodeRounded drawCell figure

drawCell :: Cell -> Widget Name
drawCell Free = withAttr emptyCellAttr $ str "· "
drawCell Filled = withAttr filledCellAttr $ str "  "

--- Attributes ---

emptyCellAttr, filledCellAttr, placingCanPlaceFullFigureAttr, placingCanPlaceButNotFullFigure, placingCannotPlaceAttr :: AttrName
emptyCellAttr = attrName "emptyCell"
filledCellAttr = attrName "filledCell"
placingCanPlaceFullFigureAttr = attrName "placingCanPlaceFullFigure"
placingCanPlaceButNotFullFigure = attrName "placingCanPlaceButNotFullFigure"
placingCannotPlaceAttr = attrName "placingCannotPlace"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [
    (emptyCellAttr, V.defAttr),
    (filledCellAttr, V.blue `on` V.blue),
    (placingCanPlaceFullFigureAttr, V.brightBlue `on` V.brightBlue),
    (placingCanPlaceButNotFullFigure, V.magenta `on` V.magenta),
    (placingCannotPlaceAttr, V.yellow `on` V.yellow)
  ]

--- Main ---

lastExceptionHandler :: SomeException -> IO ()
lastExceptionHandler e = do
  putStrLn $ "Uncaught exception: " <> displayException e

main :: IO ()
main = do
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  game <- initGame
  -- Borrowed from https://magnus.therning.org/2023-04-26-some-practical-haskell.html
  originalHandler <- getUncaughtExceptionHandler
  setUncaughtExceptionHandler $ handle originalHandler . lastExceptionHandler
  void $ customMain initialVty builder Nothing app game