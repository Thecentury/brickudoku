{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
module Blockudoku where

import Control.Lens hiding ((<|), (|>), (:>), (:<))

data Game = Game
  { _score :: Int }

makeLenses ''Game

height, width :: Int
height = 9
width = 9

initGame :: IO Game
initGame = do
  return $ Game 0