{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Undo (
    History,
    newHistory,
    put,
    undo,
    redo,
    current
) where

import Control.Lens ( makeLenses )

-- | State stacks wrapping states in time
data History s = History { 
    _current :: s, -- ^ last state putted
    _undos :: [s], -- ^ the history of putted states (reversed) without the redos
    _redos :: [s]  -- ^ history of the undo
} deriving (Show, Eq)

makeLenses ''History

-- | Create a new history with a initial state
newHistory :: s -> History s
newHistory s = History s [] []

-- | Put a new state in the history
put :: Eq s => s -> History s -> History s
put s h | s /= _current h = h { _current = s, _undos = _current h : _undos h, _redos = [] }
put _ h                  = h

-- | Undo the last state
undo :: History s -> History s
undo h@(History _ [] _) = h
undo (History current_ (u : us) redos_) = History u us (current_ : redos_)

-- | Redo the last undo
redo :: History s -> History s
redo h@(History _ _ []) = h
redo (History current_ undos_ (r : rs)) = History r (current_ : undos_) rs