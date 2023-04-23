module MyPrelude where

import Data.Array ( (!), (//), Array, Ix )

mapArrayItem :: Ix i => i -> (e -> e) -> Array i e -> Array i e
mapArrayItem index f a = a // [(index, f $ a ! index)]