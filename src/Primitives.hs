{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DerivingStrategies #-}

module Primitives where

import Linear.V2 (V2(..))

--------------------------------------------------------------------------------

type Coord = V2 Int

zeroCoord :: Coord
zeroCoord = V2 0 0

vectorUp :: Coord
vectorUp = V2 0 (-1)

vectorDown :: Coord
vectorDown = V2 0 1

vectorLeft :: Coord
vectorLeft = V2 (-1) 0

vectorRight :: Coord
vectorRight = V2 1 0

--------------------------------------------------------------------------------

data Cell =
  Free | Filled
  deriving stock (Show, Eq)

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

--------------------------------------------------------------------------------

data RangeKind =
  Horizontal |
  Vertical |
  Square
  deriving stock (Show, Eq)
