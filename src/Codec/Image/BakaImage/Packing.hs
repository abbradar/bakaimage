module Codec.Image.BakaImage.Packing where

import qualified Data.Array.Repa as R
import Data.Vector (Vector)
import qualified Data.Vector as V

import Codec.Image.BakaImage.Blocks
import Codec.Image.BakaImage.Format
import Graphics.Process.Types

packBlock :: Vector BlockInfo -> Block -> Image R.D -> Block
packBlock bis (Block bii lvl _) img' = Block bii lvl $ R.computeUnboxedS img
  where bi = bis V.! fromIntegral bii
        img = if pixel bi == R.ix2 1 1
              then img'
              else R.map (R.! R.ix2 0 0) $ blockify (pixel bi) img'

packChannel :: Channel -> Image R.D -> Channel
packChannel (Channel bis bs') img = Channel bis bs
  where bs = R.computeS $ R.map (uncurry $ packBlock bis) $ R.zipWith (,) bs' $ blockify (sbsize $ V.head bis) img
