module MyPrelude where

import Data.Array ( (!), (//), Array, Ix, assocs, array, bounds )

mapArrayItem :: Ix i => i -> (e -> e) -> Array i e -> Array i e
mapArrayItem index f a = a // [(index, f $ a ! index)]

mapiArray :: Ix i => (i -> e -> e) -> Array i e -> Array i e
mapiArray f a = array (bounds a) . map (\(i, e) -> (i, f i e)) . assocs $ a