module MyPrelude 
  ( mapArrayItem,
    mapiArray,
    (!),
    mapi,
    width2d,
    height2d,
    randomElement ) where

import Data.Array ( (//), Array, Ix, assocs, array, bounds, inRange )
import qualified Data.Array as Array
import GHC.Stack (HasCallStack)
import Linear.V2 (V2(..))
import System.Random.Stateful (StatefulGen, UniformRange (uniformRM))

--------------------------------------------------------------------------------

arrayItem :: HasCallStack => (Show i, Ix i) => Array i a -> i -> a
arrayItem a i = 
  if inRange (bounds a) i then
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

mapiArray :: Ix i => (i -> a -> b) -> Array i a -> Array i b
mapiArray f a = array (bounds a) . map (\(i, e) -> (i, f i e)) . assocs $ a

mapi :: (Int -> a -> b) -> [a] -> [b]
mapi f = zipWith f [0..]

width2d :: Array (V2 Int) a -> Int
width2d a = x2 - x1 + 1
  where
    (V2 x1 _, V2 x2 _) = bounds a

height2d :: Array (V2 Int) a -> Int
height2d a = y2 - y1 + 1
  where
    (V2 _ y1, V2 _ y2) = bounds a

----

randomElement :: StatefulGen g m => g -> [a] -> m a
randomElement gen list = do
  randomIndex <- uniformRM (0, length list - 1) gen
  pure $ list !! randomIndex
