{-# LANGUAGE TupleSections #-}

module Codec.Image.BakaImage.Decoding where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8)
import qualified Data.Array.Repa as R
import Data.Array.Repa (Array,
                        DIM2)

import Math
import Graphics.Process.Types
import Codec.Image.BakaImage.Blocks
import Codec.Image.BakaImage.Format

flattenBlock :: Vector BlockInfo -> Size -> Block -> Array R.D DIM2 ([Word8], Word8)
flattenBlock bis sz (Block bii lvl img) = (if pixel bi == R.ix2 1 1 then id else deblockifyBy dec sz (pixel bi)) $
                                         deblockify (R.extent img) (sbsize bi) $
                                         R.traverse bimg id (\i p -> R.map (V.toList $ V.map (R.! p) lvl,) $ i p)
  where bi = bis V.! fromIntegral bii
        bimg = blockify (sbsize bi) $ R.delay img
        dec (_, s) p = R.fromFunction s $ const p

flattenChannel :: Size -> Channel -> Array R.D DIM2 ([Word8], Word8)
flattenChannel sz (Channel bis d) = deblockifyBy dec sz bsize $ R.delay d
  where bsize = sbsize $ V.head bis
        dec (_, bsz) = flattenBlock bis bsz

decodeFlattened :: Array R.D DIM2 ([Word8], Word8) -> Image R.D
decodeFlattened = R.map (\(lvl, p) -> lvl !! fromIntegral p)

ybrToRGB :: Array R.D DIM2 (Word8, Word8, Word8) -> Array R.D DIM2 (Word8, Word8, Word8)
ybrToRGB = R.map (\(y, cb, cr) -> let y' :: Float
                                      y' = fromIntegral y
                                      cb' = fromIntegral cb - 128
                                      cr' = fromIntegral cr - 128

                                      r = truncTrim $ y' + 1.402 * cr'
                                      g = truncTrim $ y' - 0.34414 * cb' - 0.71414 * cr'
                                      b = truncTrim $ y' + 1.772 * cb'
                                  in (r, g, b))

decodeImage :: BakaImage -> MImage R.D
decodeImage (BakaImage sz d)
  | chs == [Y] = Grey $ V.head dd
  | chs == [Y, Cb, Cr] =
    RGB $ ybrToRGB $ R.traverse3 (dd V.! 0) (dd V.! 1) (dd V.! 2) (const $ const $ const $ sz)
    (\i1 i2 i3 p -> (i1 p, i2 p, i3 p))
  | otherwise = error "decodeImage: unknown format"
  where chs = V.toList $ V.map fst d
        dd = V.map (decodeFlattened . flattenChannel sz . snd) d
