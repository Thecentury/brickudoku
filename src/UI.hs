{-# LANGUAGE OverloadedStrings #-}
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
  , (<+>), attrName, joinBorders)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Lens ((^.), (&), (.~))
import qualified Graphics.Vty as V

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
handleEvent _ = return ()

drawUI :: Game -> [Widget Name]
drawUI game =
  [
    C.center $ drawGrid game <+> padLeft (Pad 2) (drawScore game)
  ]

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
  $ vBox cellRows
  where
    cellRows = map (hBox . map borderedCell) $ rows game

    borderedCell :: Cell -> Widget Name
    borderedCell = drawCell

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