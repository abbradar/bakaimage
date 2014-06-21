module Codec.Image.BakaImage.Size where

import Data.Array.Repa (DIM2, Z(..), (:.)(..))
import qualified Data.Array.Repa as R

type Size = (Int, Int)

toIx2 :: Size -> DIM2
toIx2 (w, h) = R.ix2 h w

fromIx2 :: DIM2 -> Size
fromIx2 (Z :. h :. w) = (w, h)
