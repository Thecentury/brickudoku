{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module UI (main) where

import Blockudoku

import Control.Monad.State.Strict
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
import Control.Lens ((^.), (&), (.~))
import qualified Graphics.Vty as V
import Data.Array (elems, Array)
import Data.Functor (void)

type Name = ()

app :: App Game () Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return ()
          , appAttrMap = const theMap
          }

handleEvent :: BrickEvent Name () -> EventM Name Game ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey V.KRight [])) =
  modify $ \game ->
    case game ^. Blockudoku.state of
      SelectingFigure -> selectNextFigure nextSelectedFigureIndex game
      PlacingFigure _ _ -> movePlacingFigure game DirRight
      _ -> game
handleEvent (VtyEvent (V.EvKey V.KLeft [])) =
  modify $ \game ->
    case game ^. Blockudoku.state of
      SelectingFigure -> selectNextFigure previousSelectedFigureIndex game
      PlacingFigure _ _ -> movePlacingFigure game DirLeft
      _ -> game
handleEvent (VtyEvent (V.EvKey V.KUp [])) =
  modify $ \game ->
    case game ^. Blockudoku.state of
      PlacingFigure _ _ -> movePlacingFigure game DirUp
      _ -> game
handleEvent (VtyEvent (V.EvKey V.KDown [])) =
  modify $ \game ->
    case game ^. Blockudoku.state of
      PlacingFigure _ _ -> movePlacingFigure game DirDown
      _ -> game
handleEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  game <- get
  game2 <-
    case game ^. Blockudoku.state of
      SelectingFigure -> pure $ startPlacingFigure game
      PlacingFigure _ _ -> liftIO $ placeFigure game
      _ -> pure game
  put game2
handleEvent (VtyEvent (V.EvKey V.KEsc [])) =
  modify $ \game ->
    case game ^. Blockudoku.state of
      PlacingFigure _ _ -> cancelPlacingFigure game
      _ -> game
handleEvent _ = return ()

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

drawPlacingCell :: PlacingCell -> Widget Name
drawPlacingCell PlacingFree = withAttr emptyCellAttr $ str "· "
drawPlacingCell PlacingFilled = withAttr filledCellAttr $ str "  "
drawPlacingCell PlacingCanPlaceFullFigure = withAttr placingCanPlaceFullFigureAttr $ str "  "
drawPlacingCell PlacingCanPlaceButNotFullFigure = withAttr placingCanPlaceButNotFullFigure $ str "  "
drawPlacingCell PlacingCannotPlace = withAttr placingCannotPlaceAttr $ str "  "

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

main :: IO ()
main = do
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  game <- initGame
  void $ customMain initialVty builder Nothing app game