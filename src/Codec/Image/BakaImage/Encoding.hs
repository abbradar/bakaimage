{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}

module Codec.Image.BakaImage.Encoding where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8)
import Data.Maybe (fromJust, isJust)
import Data.Bits (shiftL, shiftR)
import Data.Array.Repa (Array, DIM2,
                        Z(..), (:.)(..))
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.Vector as RV
import Data.Function (on)
import Data.List (find)

import Math
import Graphics.Process.Types
import Graphics.Process.Palette
import Codec.Image.BakaImage.Dithering
import Codec.Image.BakaImage.Size
import Codec.Image.BakaImage.Format
import Codec.Image.BakaImage.Blocks
import Codec.Image.BakaImage.Decoding

data EncodingInfo = EncodingInfo { palette :: Int -> [Word8] -> [Word8]
                                 , level :: Int
                                 , dither :: Image R.D -> Channel -> Channel
                                 }

paletteSubBlock :: EncodingInfo -> BlockInfo -> Image R.D -> Vector Word8
paletteSubBlock ei bi img = V.fromList $ map (\a -> a `shiftR` bitr `shiftL` bitr)
                            $ (palette ei) (fromIntegral $ levels bi) (R.toList img)
  where bitr = 8 - bitrate bi

encodeSubBlock :: Vector Word8 -> Image R.D -> Image R.D
encodeSubBlock pl = R.map (fst . quantize (map fromIntegral $ V.toList pl :: [Int]) . fromIntegral)

encodeBlock :: EncodingInfo -> Vector BlockInfo -> Word8 -> Image R.D -> Block
encodeBlock ei bis bii img' = Block bii (V.map R.computeUnboxedS pals') (R.computeUnboxedS enc)
  where img = if pixel bi == (1, 1)
              then img'
              else R.delay $ R.computeUnboxedS $ R.map (midD' . R.toList) $ blockify (pixel bi) img'
        bi = bis V.! fromIntegral bii
        bled = blockify (sbsize bi) img
        pals = R.delay $ RV.computeVectorS $ R.map (paletteSubBlock ei bi) bled
        enc = deblockify (fromIx2 $ R.extent img) (sbsize bi) $ R.zipWith encodeSubBlock pals bled
        pals' = V.map (\lv -> R.traverse pals id $ \i p -> i p V.! lv) [0..levels bi - 1]

diff :: Image R.D -> Image R.D -> [Int]
diff a b = map lvl d
  where d = R.toList $ blockify (3, 3) $ R.zipWith ((-) `on` fromIntegral) a b
        lvl img = sum $ map ((^ (2::Int)) . abs) pts
          where (Z :. h :. w) = R.extent img
                c = Z :. (h `div` 2) :. (w `div` 2)
                sgn = signum $ img R.! c
                pts = filter ((== sgn) . signum) $ R.toList img

encodeBlockOpt :: EncodingInfo -> Vector BlockInfo -> Image R.D -> Block
encodeBlockOpt ei bis img
  | lst == -1 = error "encodeBlockOpt: no block presets"
  | otherwise = encodeBlock' 0
  where lst = fromIntegral $ V.length bis - 1
        lbs = encodeBlock ei bis lst img
        org = R.delay $ R.computeUnboxedS $ decodeImg lbs

        decodeImg :: Block -> Image R.D
        decodeImg = decodeFlattened . flattenBlock bis (fromIx2 $ R.extent img)

        encodeBlock' :: Word8 -> Block
        encodeBlock' i
          | i == lst = lbs
          | otherwise =
          let cbs = encodeBlock ei bis i img
          in if isJust $ find (>= level ei) $ diff (decodeImg cbs) org
             then encodeBlock' (i + 1)
             else cbs

encodeChannel :: EncodingInfo -> Vector BlockInfo -> Image R.D -> IO Channel
encodeChannel ei bis img = do
  bs <- R.computeP $ R.map (encodeBlockOpt ei bis) $ blockify (sbsize $ V.head bis) img
  return $ (dither ei) img $ Channel bis bs

rgbToYBR :: Array R.D DIM2 (Word8, Word8, Word8) -> Array R.D DIM2 (Word8, Word8, Word8)
rgbToYBR = R.map (\(r, g, b) -> let r' :: Float
                                    r' = fromIntegral r
                                    g' = fromIntegral g
                                    b' = fromIntegral b

                                    y = truncate $ 0.299 * r' + 0.587 * g' + 0.114 * b'
                                    cb = truncate $ 128 - 0.169 * r' - 0.331 * g' + 0.500 * b'
                                    cr = truncate $ 128 + 0.500 * r' - 0.419 * g' - 0.081 * b'
                                  in (y, cb, cr))

encodeImage :: [(ChannelType, (EncodingInfo, Vector BlockInfo))] -> MImage R.D -> IO BakaImage
encodeImage cis (RGB img) = do
  ybr <- R.computeUnboxedP $ rgbToYBR img
  y <- uncurry encodeChannel (fromJust $ lookup Y cis) $ R.map (\(y,_,_) -> y) ybr
  cb <- uncurry encodeChannel (fromJust $ lookup Cb cis) $ R.map (\(_,cb,_) -> cb) ybr
  cr <- uncurry encodeChannel (fromJust $ lookup Cr cis) $ R.map (\(_,_,cr) -> cr) ybr
  return $ BakaImage (fromIx2 $ R.extent img) $ V.fromList [(Y, y), (Cb, cb), (Cr, cr)]
encodeImage cis (Grey img) = do
  y <- uncurry encodeChannel (fromJust $ lookup Y cis) img
  return $ y `seq` BakaImage (fromIx2 $ R.extent img) [(Y, y)]

standardChannel :: Vector BlockInfo
standardChannel = [ BlockInfo { levels = 2
                              , sbsize = (12, 12)
                              , pixel = (1, 1)
                              , bitrate = 8
                              }
                  , BlockInfo { levels = 2
                              , sbsize = (6, 6)
                              , pixel = (1, 1)
                              , bitrate = 8
                              }
                  , BlockInfo { levels = 2
                              , sbsize = (3, 3)
                              , pixel = (1, 1)
                              , bitrate = 8
                              }
                  , BlockInfo { levels = 4
                              , sbsize = (3, 3)
                              , pixel = (1, 1)
                              , bitrate = 8
                              }
                  ]

standardEncoding :: EncodingInfo
standardEncoding = EncodingInfo { palette = paletteMid
                                , dither = ditherED
                                , level = 50
                                }
                  
standardSettings :: [(ChannelType, (EncodingInfo, Vector BlockInfo))]
standardSettings = [ (Y, std)
                   , (Cb, std)
                   , (Cr, std)
                   ]
  where std = (standardEncoding, standardChannel)
