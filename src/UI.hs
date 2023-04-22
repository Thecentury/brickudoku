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
import Data.Array (elems)
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
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent (VtyEvent (V.EvKey V.KRight [])) =
  modify $ \game ->
    case game ^. Blockudoku.state of
      SelectingFigure -> selectNextFigure nextSelectedFigureIndex game
      _ -> game
handleEvent (VtyEvent (V.EvKey V.KLeft [])) =
  modify $ \game ->
    case game ^. Blockudoku.state of
      SelectingFigure -> selectNextFigure previousSelectedFigureIndex game
      _ -> game
handleEvent _ = return ()

drawUI :: Game -> [Widget Name]
drawUI game =
  [
    C.center (centralColumn <+> padLeft (Pad 2) (drawScore game))
  ] where
    centralColumn = C.hCenter (drawGrid game) <=> padTop (Pad 1) figuresToPlace
    figuresToPlace = C.hCenter $ withBorderStyle BS.unicodeRounded $ B.border $ hBox $ map (vLimit 6 . C.vCenter . drawFigureToPlace) $ elems $ game ^. figures

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
  $ drawFigure (game ^. board)

drawFigure :: Figure -> Widget Name
drawFigure figure = vBox cellRows where
  cellRows = map (hBox . map drawCell) $ figureRows figure

selectedFigureBorderMappings :: [(A.AttrName, V.Attr)]
selectedFigureBorderMappings =
    [ (B.borderAttr, fg V.yellow) ]

notSelectedFigureBorderMappings :: [(A.AttrName, V.Attr)]
notSelectedFigureBorderMappings =
    [ (B.borderAttr, fg V.white) ]

drawSomeFigureToPlace :: [(A.AttrName, V.Attr)] -> BS.BorderStyle -> Figure -> Widget Name
drawSomeFigureToPlace mapping borderStyle figure =
 padLeftRight 2
 $ updateAttrMap (A.applyAttrMappings mapping)
 $ withBorderStyle borderStyle
 $ B.border
 $ hLimit 10
 $ vLimit 6
 $ C.center
 $ drawFigure figure

drawFigureToPlace :: Maybe (Selectable Figure) -> Widget Name
drawFigureToPlace Nothing = padAll 3 emptyWidget
drawFigureToPlace (Just (Selected figure)) = drawSomeFigureToPlace selectedFigureBorderMappings BS.unicodeBold figure
drawFigureToPlace (Just (NotSelected figure)) = drawSomeFigureToPlace notSelectedFigureBorderMappings BS.unicodeRounded figure

drawCell :: Cell -> Widget Name
drawCell Free = withAttr emptyCellAttr $ str "· "
drawCell Filled = withAttr filledCellAttr $ str "  "

--- Attributes ---

emptyCellAttr, filledCellAttr :: AttrName
emptyCellAttr = attrName "emptyCell"
filledCellAttr = attrName "filledCell"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [
    (emptyCellAttr, V.defAttr),
    (filledCellAttr, V.blue `on` V.blue)
  ]

--- Main ---

main :: IO ()
main = do
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  game <- initGame
  void $ customMain initialVty builder Nothing app game