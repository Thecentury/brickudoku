module MyPrelude 
  ( mapArrayItem,
    mapiArray,
    (!) ) where

import Data.Array ( (//), Array, Ix, assocs, array, bounds )
import qualified Data.Array as Array
import GHC.Stack (HasCallStack)

arrayItem :: HasCallStack => (Show i, Ix i) => Array i a -> i -> a
arrayItem a i = 
  if Array.inRange (bounds a) i then
    a Array.! i
  else
    error $ "Index " ++ show i ++ " out of bounds [" ++ show minIndex ++ ", " ++ show maxIndex ++ "]"
  where
    (minIndex, maxIndex) = bounds a

-- Operator for the `arrayItem` function
infixl 9 !
(!) :: (HasCallStack, Show i, Ix i) => Array i a -> i -> a
(!) = arrayItem

mapArrayItem :: (HasCallStack, Ix i, Show i) => i -> (e -> e) -> Array i e -> Array i e
mapArrayItem index f a = a // [(index, f $ a ! index)]

mapiArray :: Ix i => (i -> e -> e) -> Array i e -> Array i e
mapiArray f a = array (bounds a) . map (\(i, e) -> (i, f i e)) . assocs $ a