{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Undo (
    History(..),
    newHistory,
    put,
    tryUndo,
    tryUndoUntil,
    tryUndoUntilDifferentL,
    tryRedo,
    tryRedoUntil,
    tryRedoUntilDifferentL,
    current
) where

import Control.Lens ( makeLenses, (^.), Lens' )
import GHC.Generics ( Generic )

--------------------------------------------------------------------------------

-- | State stacks wrapping states in time
data History s = History { 
    _current :: s, -- ^ last state putted
    _undos :: [s], -- ^ the history of putted states (reversed) without the redos
    _redos :: [s]  -- ^ history of the undo
} deriving (Show, Eq, Generic)

makeLenses ''History

-- | Create a new history with an initial state.
newHistory :: s -> History s
newHistory s = History s [] []

-- | Put a new state in the history
put :: Eq s => s -> History s -> History s
put s h | s /= _current h = h { _current = s, _undos = _current h : _undos h, _redos = [] }
put _ h                  = h

tryUndo :: History s -> Maybe (History s)
tryUndo (History _ [] _) = Nothing
tryUndo (History current_ (u : us) redos_) = Just $ History u us (current_ : redos_)

tryUndoUntil :: (s -> s -> Bool) -> History s -> Maybe (History s)
tryUndoUntil f h = case tryUndo h of
    Nothing -> Nothing
    Just h' ->
      if f (h' ^. current) (h ^. current) then 
        Just h'
      else
        tryUndoUntil f h'

tryUndoUntilDifferentL :: forall s l. Eq l => Lens' s l -> History s -> Maybe (History s)
tryUndoUntilDifferentL l = tryUndoUntil (\curr prev -> curr ^. l /= prev ^. l)

tryRedo :: History s -> Maybe (History s)
tryRedo (History _ _ []) = Nothing
tryRedo (History current_ undos_ (r : rs)) = Just $ History r (current_ : undos_) rs

tryRedoUntil :: (s -> s -> Bool) -> History s -> Maybe (History s)
tryRedoUntil f h = case tryRedo h of
    Nothing -> Nothing
    Just h' ->
      if f (h' ^. current) (h ^. current) then 
        Just h'
      else
        tryRedoUntil f h'

tryRedoUntilDifferentL :: forall s l. Eq l => Lens' s l -> History s -> Maybe (History s)
tryRedoUntilDifferentL l = tryRedoUntil (\curr prev -> curr ^. l /= prev ^. l)