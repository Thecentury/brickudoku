{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UI (main) where

import Brickudoku
  ( Game,
    Figure,
    Cell(..),
    Coord,
    UserAction(..),
    SystemAction(..),
    Clickable(..),
    Action(..),
    score,
    turnNumber,
    emptyFigure,
    initGame,
    figureRows,
    possibleActions,
    isGameOver,
    isPlacingFigure,
    autoPlay,
    GameEvent (..),
    FigureInSelection(..),
    currentGame, hoverOver )
import VisualBoard
  ( VisualCell(..),
    FreeStyle(..),
    cellsToDisplay,
    figuresToPlace,
    FigureToPlace(..),
    FigureToPlaceKind(..),
    HintPlacementResult(..) )
import Persistence (saveToFile, loadFromFileIfExists)

import Control.Monad.State.Strict ( MonadState(put, get) )
import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Widget
  , customMain, neverShowCursor
  , halt
  , hLimit, vBox, hBox, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle, str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg, bg
  , (<+>), (<=>), attrName, joinBorders, padLeftRight, vLimit, updateAttrMap, clickable, getVtyHandle )
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Types as T
import Control.Lens ((^.))
import qualified Graphics.Vty as V
import Data.Array (Array)
import Data.Functor (void)
import GHC.Stack (HasCallStack)
import GHC.Conc.Sync (getUncaughtExceptionHandler, setUncaughtExceptionHandler)
import Control.Exception (SomeException, Exception (displayException), handle)
import Data.Map (Map)
import qualified Data.Map as Map
import Brick.BChan (newBChan, writeBChan)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad ( forever, when )
import System.Random.Stateful (runStateGen, StdGen, initStdGen)
import Control.Monad.IO.Class (liftIO)

newtype Name = Name Clickable
  deriving newtype (Show, Eq, Ord)

app :: App FullGameState GameEvent Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = do
              vty <- getVtyHandle
              let output = V.outputIface vty
              when (V.supportsMode output V.Mouse) $
                -- todo check if it is possible to be notified about mouse move events
                liftIO $ V.setMode output V.Mouse True
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
    (VtyEvent (V.EvKey (V.KChar 'H') []), [SystemAction ToggleEasyMode]),
    (VtyEvent (V.EvKey (V.KChar 'u') []), [SystemAction Undo]),
    (VtyEvent (V.EvKey (V.KChar 'r') []), [SystemAction Redo]),
    (AppEvent Tick, [SystemAction NextAutoPlayTurn])
  ]

handleEvent :: HasCallStack => BrickEvent Name GameEvent -> EventM Name FullGameState ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'Q') [])) = do
  (FullGameState game gen) <- get
  liftIO $ saveToFile game gen
  halt

handleEvent (T.MouseDown (Name name) V.BLeft [] _) = do
  (FullGameState game gen) <- get
  let (actions, gen') = runStateGen gen (possibleActions game)
  let actionsToApply = filter (\(a, _) -> a == Click name) actions
  case actionsToApply of
    [] -> pure ()
    [(_, game')] -> put $ FullGameState game' gen'
    _ -> error $ "Multiple applicable actions for click " ++ show name ++ ": " ++ show (fmap fst actionsToApply)

handleEvent (T.MouseDown (Name name) V.BRight [] _) = do
  (FullGameState game gen) <- get
  let game' = hoverOver game name
  put $ FullGameState game' gen

handleEvent evt = do
  (FullGameState game gen) <- get
  let (actions, gen') = runStateGen gen (possibleActions game)
  case Map.lookup evt keyBindings of
    Just applicableKeyBindingActions ->
      let
        actionsToApply = filter (\(a, _) -> a `elem` applicableKeyBindingActions) actions
      in
        case actionsToApply of
          [] -> pure ()
          [(_, game')] -> put $ FullGameState game' gen'
          _ -> error $ "Multiple applicable actions for key " ++ show evt ++ ": " ++ show (fmap fst actionsToApply)
    Nothing -> pure ()

drawUI :: HasCallStack => FullGameState -> [Widget Name]
drawUI (FullGameState game _) =
  [
    gameOverWidget,
    C.center $ withAutoPlayStyle $ applyGameOverPalette $ centralColumn <+> padTop rightColumnTopPadding (padLeft (Pad 2) rightColumn)
  ] where
    centralColumn =
      wrapWithAutoPlayBorder
      $ C.hCenter (str $ "Turn: " ++ show (game ^. currentGame . turnNumber))
      <=> C.hCenter (withPlacingFigureBorder $ drawGrid game)
      <=> padTop (Pad 1) figuresToPlaceWidgets

    rightColumn =
      vBox [
        drawScore game,
        padTop (Pad 2) helpWidget
      ]

    rightColumnTopPadding =
      if game ^. autoPlay then
        Pad 3
      else
        Pad 0

    withAutoPlayStyle =
      if game ^. autoPlay then
        updateAttrMap (A.applyAttrMappings autoPlayBorderMapping)
      else
        id

    figuresToPlaceWidgets =
      C.hCenter
      $ withBorderStyle BS.unicodeRounded
      $ B.border
      $ hBox
      $ map (withClickableId $ vLimit 6 . C.vCenter . drawFigureToPlace)
      $ figuresToPlace game

    withClickableId :: (Maybe a -> Widget Name) -> Maybe (a, Clickable) -> Widget Name
    withClickableId render Nothing = render Nothing
    withClickableId render (Just (a, name)) = clickable (Name name) $ render $ Just a

    applyGameOverPalette widget =
      if isGameOver game then
        updateAttrMap (A.applyAttrMappings gameOverMap) widget
      else
        widget

    gameOverWidget =
      if isGameOver game then
        C.centerLayer
        $ withAttr gameOverAttr
        $ withBorderStyle BS.unicodeRounded
        $ B.border
        $ hLimit 15
        $ vLimit 5
        $ C.center
        $ str "Game over"
      else
         emptyWidget

    withPlacingFigureBorder :: Widget Name -> Widget Name
    withPlacingFigureBorder widget =
      if isPlacingFigure game then
        updateAttrMap (A.applyAttrMappings selectedPlacingFigureBorderMappings) widget
      else
        widget

    autoPlayBorderMapping =
      [(B.borderAttr, fg V.brightRed)]

    wrapWithAutoPlayBorder :: Widget Name -> Widget Name
    wrapWithAutoPlayBorder widget =
      if game ^. autoPlay then
        hLimit 60
        $ C.hCenter
        $ withBorderStyle BS.unicodeRounded
        $ B.borderWithLabel (str " Auto-play ")
        $ padAll 2 widget
      else
        hLimit 60 widget

drawScore :: Game -> Widget Name
drawScore game =
  padTop (Pad 1)
  $ hLimit 9
  $ withBorderStyle BS.unicodeRounded
  $ B.borderWithLabel (str " Score ")
  $ C.hCenter
  $ padAll 1
  $ str scoreString where
    scoreString =
      if length string `div` 2 == 0 then
        string
      else
        " " ++ string
    string = show $ game ^. currentGame . score

helpWidget :: Widget Name
helpWidget =
  withBorderStyle BS.unicodeRounded
  $ B.borderWithLabel (str " Help ")
  $ padAll 1
  $ vBox helpLines where
    helpLines :: [Widget Name]
    helpLines =
      [
        str "Move figure: . . . . . " <+> key "←" <+> str ", " <+> key "→" <+> str ", " <+> key "↑" <+> str ", " <+> key "↓",
        str "Place figure: . . . .  " <+> key "Enter",
        str "Cancel placing figure: " <+> key "Esc",
        str "Restart game: . . . .  " <+> key "Shift" <+> str " + " <+> key "R",
        str "Toggle auto-play: . .  " <+> key "Shift" <+> str " + " <+> key "A",
        str "Toggle hints: . . . .  " <+> key "Shift" <+> str " + " <+> key "H",
        str "Undo: . . . . . . . .  " <+> key "U",
        str "Redo: . . . . . . . .  " <+> key "R",
        str "Quit: . . . . . . . .  " <+> key "Shift" <+> str " + " <+> key "Q"
      ]
    key k = withAttr helpShortcutAttr $ str k

drawGrid :: HasCallStack => Game -> Widget Name
drawGrid game =
  withBorderStyle BS.unicodeRounded
  $ joinBorders
  $ B.border
  $ padAll 0
  $ drawFigure drawPlacingCell ClickableCell
  $ cellsToDisplay game

cellWidget :: Widget n
cellWidget = str "  "

hintWidget :: Widget n
hintWidget = str "··"

hintWillFreeWidget :: Widget n
hintWillFreeWidget = str "◤◢"

drawPlacingCell :: VisualCell -> Widget Name
drawPlacingCell (VFree PrimaryStyle) = withAttr emptyCellAttr cellWidget
drawPlacingCell (VFree AltStyle)     = withAttr emptyAltStyleCellAttr cellWidget
drawPlacingCell VFilled = withAttr filledCellAttr cellWidget
drawPlacingCell VCanPlaceFullFigure = withAttr placingCanPlaceFullFigureAttr cellWidget
drawPlacingCell VCanPlaceButNotFullFigure = withAttr placingCanPlaceButNotFullFigure cellWidget
drawPlacingCell VCannotPlace = withAttr placingCannotPlaceAttr cellWidget
drawPlacingCell VWillBeFreed = withAttr placingWillBeFreedAttr cellWidget
drawPlacingCell (VCanBePlacedHint PrimaryStyle JustFigure) = withAttr canBePlacedHintAttr hintWidget
drawPlacingCell (VCanBePlacedHint AltStyle     JustFigure) = withAttr canBePlacedHintAltStyleAttr hintWidget
drawPlacingCell (VCanBePlacedHint PrimaryStyle Region)     = withAttr canBePlacedWillFreeHintAttr hintWillFreeWidget
drawPlacingCell (VCanBePlacedHint AltStyle     Region)     = withAttr canBePlacedWillFreeHintAltStyleAttr hintWillFreeWidget

data DrawFigureCellKind = ClickableCell | NotClickableCell deriving (Show, Eq)

drawFigure :: HasCallStack => (a -> Widget Name) -> DrawFigureCellKind -> Array Coord a -> Widget Name
drawFigure drawOneCell ClickableCell figure = vBox cellRows where
  cellRows = hBox . map (\(cell, clickableId) -> clickable (Name clickableId) $ drawOneCell cell) <$> figureRows figure
drawFigure drawOneCell NotClickableCell figure = vBox cellRows where
  cellRows = hBox . map (drawOneCell . fst) <$> figureRows figure

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

drawSomeFigureToPlace :: HasCallStack => [(A.AttrName, V.Attr)] -> BS.BorderStyle -> (Cell -> Widget Name) -> Figure -> Widget Name
drawSomeFigureToPlace mapping borderStyle drawOneCell figure =
  padLeftRight 2
  $ updateAttrMap (A.applyAttrMappings mapping)
  $ withBorderStyle borderStyle
  $ B.border
  $ hLimit 10
  $ vLimit 6
  $ C.center
  $ drawFigure drawOneCell NotClickableCell figure

drawFigureToPlace :: HasCallStack => Maybe (FigureToPlace FigureInSelection) -> Widget Name
drawFigureToPlace Nothing                                                             = drawSomeFigureToPlace notSelectedCanBePlacedFigureBorderMappings BS.unicodeRounded (\_ -> withAttr emptyCellAttr $ str "  ") emptyFigure
drawFigureToPlace (Just (FigureToPlace (FigureInSelection figure _) Selected))        = drawSomeFigureToPlace selectedFigureBorderMappings BS.unicodeBold drawCell figure
drawFigureToPlace (Just (FigureToPlace (FigureInSelection figure _) SelectedPlacing)) = drawSomeFigureToPlace selectedPlacingFigureBorderMappings BS.unicodeBold drawCell figure
drawFigureToPlace (Just (FigureToPlace (FigureInSelection figure _) CanBePlaced))     = drawSomeFigureToPlace notSelectedCanBePlacedFigureBorderMappings BS.unicodeRounded drawCell figure
drawFigureToPlace (Just (FigureToPlace (FigureInSelection figure _) CannotBePlaced))  = drawSomeFigureToPlace notSelectedCanNotBePlacedFigureBorderMappings BS.unicodeRounded drawCell figure

drawCell :: Cell -> Widget Name
drawCell Free = withAttr emptyCellAttr cellWidget
drawCell Filled = withAttr filledCellAttr cellWidget

--- Attributes ---

emptyCellAttr, emptyAltStyleCellAttr, filledCellAttr, placingCanPlaceFullFigureAttr,
  placingCanPlaceButNotFullFigure, placingCannotPlaceAttr, placingWillBeFreedAttr,
  board3x3BorderAttr,
  canBePlacedHintAttr, canBePlacedHintAltStyleAttr,
  canBePlacedWillFreeHintAttr, canBePlacedWillFreeHintAltStyleAttr,
  helpShortcutAttr, gameOverAttr :: AttrName
emptyCellAttr                       = attrName "emptyCell"
emptyAltStyleCellAttr               = attrName "emptyAltStyleCell"
filledCellAttr                      = attrName "filledCell"
placingCanPlaceFullFigureAttr       = attrName "placingCanPlaceFullFigure"
placingCanPlaceButNotFullFigure     = attrName "placingCanPlaceButNotFullFigure"
placingCannotPlaceAttr              = attrName "placingCannotPlace"
placingWillBeFreedAttr              = attrName "placingWillBeFreed"
board3x3BorderAttr                  = attrName "board3x3Border"
canBePlacedHintAttr                 = attrName "canBePlacedHint"
canBePlacedHintAltStyleAttr         = attrName "canBePlacedHintAltStyle"
canBePlacedWillFreeHintAttr         = attrName "canBePlacedWillFreeHint"
canBePlacedWillFreeHintAltStyleAttr = attrName "canBePlacedWillFreeHintAltStyle"
helpShortcutAttr                    = attrName "helpShortcut"
gameOverAttr                        = attrName "gameOver"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [
    (emptyCellAttr, V.defAttr),
    (emptyAltStyleCellAttr, bg V.brightWhite),
    (filledCellAttr, V.blue `on` V.blue),
    (placingCanPlaceFullFigureAttr, V.brightBlue `on` V.brightBlue),
    (placingCanPlaceButNotFullFigure, V.magenta `on` V.magenta),
    (placingCannotPlaceAttr, V.yellow `on` V.yellow),
    (placingWillBeFreedAttr, V.green `on` V.green),
    (board3x3BorderAttr, fg V.white),
    -- todo somehow combine these 4.
    (canBePlacedHintAttr, fg V.green),
    (canBePlacedHintAltStyleAttr, V.green `on` V.brightWhite),
    (canBePlacedWillFreeHintAttr, fg V.yellow),
    (canBePlacedWillFreeHintAltStyleAttr, V.yellow `on` V.brightWhite),
    (gameOverAttr, fg V.red),
    (helpShortcutAttr, fg V.blue)
  ]

gameOverMap :: [(AttrName, V.Attr)]
gameOverMap =
  let
    disabled = V.white `on` V.white
  in
  [
    (emptyCellAttr, V.defAttr),
    (emptyAltStyleCellAttr, V.defAttr),
    (filledCellAttr, disabled),
    (placingCanPlaceFullFigureAttr, disabled),
    (placingCanPlaceButNotFullFigure, disabled),
    (placingCannotPlaceAttr, disabled),
    (placingWillBeFreedAttr, disabled),
    (board3x3BorderAttr, disabled),
    (canBePlacedHintAttr, disabled),
    (canBePlacedHintAltStyleAttr, V.defAttr),
    (canBePlacedWillFreeHintAttr, disabled),
    (canBePlacedWillFreeHintAltStyleAttr, V.defAttr),
    (gameOverAttr, fg V.brightRed),
    (helpShortcutAttr, fg V.brightBlack)
  ]

data FullGameState = FullGameState
  { _game :: Game,
    _rng :: StdGen }

--- Main ---

lastExceptionHandler :: SomeException -> IO ()
lastExceptionHandler e = do
  putStrLn $ "Uncaught exception: " <> displayException e

loadOrInitGame :: HasCallStack => IO FullGameState
loadOrInitGame = do
  loadResult <- loadFromFileIfExists
  case loadResult of
    Nothing -> newGame
    Just (Left err) -> do
      putStrLn $ "Error loading game: " <> err
      newGame
    Just (Right (game, gen)) -> return $ FullGameState game gen
  where
    newGame :: IO FullGameState
    newGame = do
      gen <- initStdGen
      let (game, gen') = runStateGen gen initGame
      return $ FullGameState game gen'

-- todo pass external RNG seed
main :: HasCallStack => IO ()
main = do
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  fullGame <- loadOrInitGame
  -- Idea borrowed from https://magnus.therning.org/2023-04-26-some-practical-haskell.html
  originalHandler <- getUncaughtExceptionHandler
  setUncaughtExceptionHandler $ handle originalHandler . lastExceptionHandler
  chan <- newBChan 10
  let delay = 100_000 -- 100 ms
  void . forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay delay

  void $ customMain initialVty builder (Just chan) app fullGame