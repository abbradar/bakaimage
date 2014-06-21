module Codec.Image.BakaImage.Packing where

import qualified Data.Array.Repa as R
import qualified Data.Vector as V

import Codec.Image.BakaImage.Blocks
import Codec.Image.BakaImage.Format
import Graphics.Process.Types

packBlock :: Block -> Image R.D -> Block
packBlock (Block bii lvl _) img = Block bii lvl $ R.computeUnboxedS img

packChannel :: Channel -> Image R.D -> Channel
packChannel (Channel bis bs') img = Channel bis bs
  where bs = R.computeS $ R.map (uncurry $ packBlock) $ R.zipWith (,) bs' $ blockify (sbsize $ V.head bis) img
