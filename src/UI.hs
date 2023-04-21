{-# LANGUAGE OverloadedStrings #-}
module UI (main) where

import Blockudoku

import Control.Monad.State.Strict
import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Widget
  , customMain, neverShowCursor
  , halt
  , hLimit, vBox, hBox  , padRight, padTop, padAll, Padding(..)
  , withBorderStyle, str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>), attrName)
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
    C.center $ drawScore game
  ]

drawScore :: Game -> Widget Name
drawScore game = withBorderStyle BS.unicodeRounded
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show $ game ^. score

theMap :: AttrMap
theMap = attrMap V.defAttr []

main :: IO ()
main = do
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  game <- initGame
  void $ customMain initialVty builder Nothing app game