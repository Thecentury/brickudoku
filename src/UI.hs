{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module UI (main) where

import Blockudoku
    ( Game,
      GameState(..),
      Figure,
      CellCoord,
      PlacingCell(..),
      Cell(..),
      cellsToDisplay,
      UserAction(..),
      SystemAction(..),
      Action(..),
      FigureToPlace(..),
      FigureToPlaceKind(..),
      board,
      figuresToPlace,
      score,
      -- todo remove
      state,
      turnNumber,
      emptyFigure,
      initGame,
      figureRows,
      possibleActions,
      GameEvent (..),
      figureInSelection,
      FigureInSelection )

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
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever)

type Name = ()

app :: App Game GameEvent Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return ()
          , appAttrMap = const theMap
          }

keyBindings :: Map (BrickEvent Name GameEvent) [Action]
keyBindings =
  Map.fromList [
    (VtyEvent (V.EvKey V.KRight []), [UserAction MoveFigureRight, UserAction SelectNextFigure]),
    (VtyEvent (V.EvKey V.KLeft []), [UserAction MoveFigureLeft, UserAction SelectPreviousFigure]),
    (VtyEvent (V.EvKey V.KUp []), [UserAction MoveFigureUp]),
    (VtyEvent (V.EvKey V.KDown []), [UserAction MoveFigureDown]),
    (VtyEvent (V.EvKey V.KEnter []), [UserAction StartPlacingFigure, UserAction PlaceFigure]),
    (VtyEvent (V.EvKey V.KEsc []), [UserAction CancelPlacingFigure]),
    (VtyEvent (V.EvKey (V.KChar 'R') []), [SystemAction RestartGame]),
    (VtyEvent (V.EvKey (V.KChar 'A') []), [SystemAction ToggleAutoPlay]),
    (AppEvent Tick, [SystemAction NextAutoPlayTurn])
  ]

handleEvent :: BrickEvent Name GameEvent -> EventM Name Game ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'Q') [])) = halt
handleEvent evt = do
  game <- get
  actions <- liftIO $ possibleActions game
  case Map.lookup evt keyBindings of
    Just applicableKeyBindingActions ->
      let
        actionsToApply = filter (\(a, _) -> a `elem` applicableKeyBindingActions) actions
      in
        case actionsToApply of
          [] -> pure ()
          [(_, newGame)] -> put newGame
          _ -> error $ "Multiple applicable actions for key " ++ show evt ++ ": " ++ show (fmap fst actionsToApply)
    Nothing -> pure ()

-- todo display whether an auto play is enabled
drawUI :: Game -> [Widget Name]
drawUI game =
  [
    gameOverWidget,
    C.center (centralColumn <+> padLeft (Pad 2) (drawScore game))
  ] where
    centralColumn =
      applyGameOverPalette
      $ C.hCenter (str $ "Turn: " ++ show (game ^. turnNumber))
      <=> C.hCenter (drawGrid game)
      <=> padTop (Pad 1) figuresToPlaceWidgets
    figuresToPlaceWidgets =
      C.hCenter
      $ withBorderStyle BS.unicodeRounded
      $ B.border
      $ hBox
      $ map (vLimit 6 . C.vCenter . drawFigureToPlace) $ figuresToPlace game
    applyGameOverPalette widget =
      case game ^. state of
        GameOver -> updateAttrMap (A.applyAttrMappings gameOverMap) widget
        _ -> widget
    gameOverWidget = 
      case game ^. state of
        GameOver ->
          C.centerLayer $ withAttr gameOverAttr $ withBorderStyle BS.unicodeRounded $ B.border $ hLimit 15 $ vLimit 5 $ C.center $ str "Game over"
        _ -> emptyWidget

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
  $ drawFigure drawPlacingCell
  $ cellsToDisplay game

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

notSelectedCanBePlacedFigureBorderMappings :: [(A.AttrName, V.Attr)]
notSelectedCanBePlacedFigureBorderMappings =
    [ (B.borderAttr, fg V.white) ]

notSelectedCanNotBePlacedFigureBorderMappings :: [(A.AttrName, V.Attr)]
notSelectedCanNotBePlacedFigureBorderMappings =
    [
      (B.borderAttr, fg V.white),
      (filledCellAttr, V.white `on` V.white)
    ]

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

drawFigureToPlace :: Maybe (FigureToPlace FigureInSelection) -> Widget Name
drawFigureToPlace Nothing                                       = drawSomeFigureToPlace notSelectedCanBePlacedFigureBorderMappings BS.unicodeRounded (\_ -> withAttr emptyCellAttr $ str "  ") emptyFigure
drawFigureToPlace (Just (FigureToPlace figure Selected))        = drawSomeFigureToPlace selectedFigureBorderMappings BS.unicodeBold drawCell $ figure ^. figureInSelection
drawFigureToPlace (Just (FigureToPlace figure SelectedPlacing)) = drawSomeFigureToPlace selectedPlacingFigureBorderMappings BS.unicodeBold drawCell $ figure ^. figureInSelection
drawFigureToPlace (Just (FigureToPlace figure CanBePlaced))     = drawSomeFigureToPlace notSelectedCanBePlacedFigureBorderMappings BS.unicodeRounded drawCell $ figure ^. figureInSelection
drawFigureToPlace (Just (FigureToPlace figure CannotBePlaced))  = drawSomeFigureToPlace notSelectedCanNotBePlacedFigureBorderMappings BS.unicodeRounded drawCell $ figure ^. figureInSelection

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

gameOverAttr :: AttrName
gameOverAttr = attrName "gameOver"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [
    (emptyCellAttr, V.defAttr),
    (filledCellAttr, V.blue `on` V.blue),
    (placingCanPlaceFullFigureAttr, V.brightBlue `on` V.brightBlue),
    (placingCanPlaceButNotFullFigure, V.magenta `on` V.magenta),
    (placingCannotPlaceAttr, V.yellow `on` V.yellow),
    (gameOverAttr, fg V.red)
  ]

gameOverMap :: [(AttrName, V.Attr)]
gameOverMap =
  [
    (emptyCellAttr, V.defAttr),
    (filledCellAttr, V.white `on` V.white),
    (placingCanPlaceFullFigureAttr, V.white `on` V.white),
    (placingCanPlaceButNotFullFigure, V.white `on` V.white),
    (placingCannotPlaceAttr, V.white `on` V.white),
    (gameOverAttr, fg V.brightRed)
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
  chan <- newBChan 10
  -- todo increase the delay (to 100 ms?)
  let delay = 20_000 -- 20 ms
  void . forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay delay

  void $ customMain initialVty builder (Just chan) app game