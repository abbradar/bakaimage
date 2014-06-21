{-# LANGUAGE TupleSections #-}

module Codec.Image.BakaImage.Decoding where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8)
import qualified Data.Array.Repa as R
import Data.Array.Repa (Array,
                        DIM2)

import Graphics.Process.Types
import Codec.Image.BakaImage.Blocks
import Codec.Image.BakaImage.Size
import Codec.Image.BakaImage.Format

flattenBlock :: Vector BlockInfo -> Size -> Block -> Array R.D DIM2 ([Word8], Word8)
flattenBlock bis sz (Block bii lvl img) = (if pixel bi == (1, 1) then id else deblockifyBy dec sz (pixel bi)) $
                                         deblockify (fromIx2 $ R.extent img) (sbsize bi) $
                                         R.traverse bimg id (\i p -> R.map (V.toList $ V.map (R.! p) lvl,) $ i p)
  where bi = bis V.! fromIntegral bii
        bimg = blockify (sbsize bi) $ R.delay img
        dec (_, s) p = R.fromFunction (toIx2 s) $ const p

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

                                      r = truncate $ y' + 1.400 * cr'
                                      g = truncate $ y' - 0.343 * cb' - 0.711 * cr'
                                      b = truncate $ y' + 1.765 * cb'
                                  in (r, g, b))

decodeImage :: BakaImage -> MImage R.D
decodeImage (BakaImage sz d)
  | chs == [Y] = Grey $ V.head dd
  | chs == [Y, Cb, Cr] =
    RGB $ ybrToRGB $ R.traverse3 (dd V.! 0) (dd V.! 1) (dd V.! 2) (const $ const $ const $ toIx2 sz)
    (\i1 i2 i3 p -> (i1 p, i2 p, i3 p))
  | otherwise = error "decodeImage: unknown format"
  where chs = V.toList $ V.map fst d
        dd = V.map (decodeFlattened . flattenChannel sz . snd) d
