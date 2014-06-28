{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}

module Codec.Image.BakaImage.Encoding where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8)
import Data.Maybe (fromJust)
import Data.Bits (shiftL, shiftR)
import Data.Array.Repa (Array, DIM2,
                        Z(..), (:.)(..))
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.Vector as RV
import Data.Function (on)
import Data.List (find)

import Math
import Graphics.Process.Types
import Codec.Image.BakaImage.Format
import Codec.Image.BakaImage.Blocks
import Codec.Image.BakaImage.Decoding

data EncodingInfo = EncodingInfo { palette :: Int -> [Int] -> [Int]
                                 , threshold :: Float
                                 , errorK :: Float
                                 , dither :: Image R.D -> Channel -> Channel
                                 }

paletteSubBlock :: EncodingInfo -> BlockInfo -> Image R.D -> Vector Word8
paletteSubBlock ei bi img = V.fromList $ map (\a -> (fromIntegral a) `shiftR` bitr `shiftL` bitr)
                            $ (palette ei) (fromIntegral $ levels bi) (map fromIntegral $ R.toList img)
  where bitr = 8 - bitrate bi

encodeSubBlock :: Vector Word8 -> Image R.D -> Image R.D
encodeSubBlock pl = R.map (fst . quantize (map fromIntegral $ V.toList pl :: [Int]) . fromIntegral)

encodeBlock :: EncodingInfo -> Vector BlockInfo -> Word8 -> Image R.D -> Block
encodeBlock ei bis bii img' = Block bii (V.map R.computeUnboxedS pals') (R.computeUnboxedS enc)
  where img = if pixel bi == R.ix2 1 1
              then img'
              else R.delay $ R.computeUnboxedS $ R.map pixelize $ blockify (pixel bi) img'
        pixelize = (fromIntegral :: Int -> Word8) . midD . (map fromIntegral) . R.toList
        bi = bis V.! fromIntegral bii
        bled = blockify (sbsize bi) img
        pals = R.delay $ RV.computeVectorS $ R.map (paletteSubBlock ei bi) bled
        enc = deblockify (R.extent img) (sbsize bi) $ R.zipWith encodeSubBlock pals bled
        pals' = V.map (\lv -> R.traverse pals id $ \i p -> i p V.! lv) [0..levels bi - 1]

diff :: Image R.D -> Image R.D -> [Float]
diff a b = map fromIntegral d
  where d = map lvl $ R.toList $ blockify (R.ix2 3 3) $ R.zipWith ((-) `on` fromIntegral) a b
        lvl img = let (Z :. h :. w) = R.extent img
                      c = img R.! (Z :. (h `div` 2) :. (w `div` 2))
                  in case find (/= 0) $ c : R.toList img of
                    Nothing -> 0 :: Int
                    Just x -> let sgn = if x >= 0 then (>= 0) else (<= 0)
                                  pts = filter sgn $ R.toList img
                              in sum $ map abs pts

encodeBlockOpt :: EncodingInfo -> Vector BlockInfo -> Image R.D -> Block
encodeBlockOpt ei bis img
  | lst == -1 = error "encodeBlockOpt: no block presets"
  | otherwise = encodeBlock' 0
  where lst = fromIntegral $ V.length bis - 1
        lbs = encodeBlock ei bis lst img
        err = map (max (threshold ei) . (* errorK ei)) $ diff img (decodeImg lbs)

        decodeImg :: Block -> Image R.D
        decodeImg = decodeFlattened . flattenBlock bis (R.extent img)

        encodeBlock' :: Word8 -> Block
        encodeBlock' i
          | i == lst = lbs
          | otherwise =
          let cbs = encodeBlock ei bis i img
              err' = zip err $ map (max $ threshold ei) $ diff img $ decodeImg cbs
          in if any (uncurry (<)) err'
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

                                    y = truncTrim $ 0.299 * r' + 0.587 * g' + 0.114 * b'
                                    cb = truncTrim $ 128 + ((-0.1687) * r' - 0.3313 * g' + 0.5 * b')
                                    cr = truncTrim $ 128 + (0.5 * r' - 0.4187 * g' - 0.0813 * b')
                                  in (y, cb, cr))

type Encoding = [(ChannelType, (EncodingInfo, Vector BlockInfo))]

encodeImage :: Encoding -> MImage R.D -> IO BakaImage
encodeImage cis (RGB img) = do
  ybr <- R.computeUnboxedP $ rgbToYBR img
  y <- uncurry encodeChannel (fromJust $ lookup Y cis) $ R.map (\(y,_,_) -> y) ybr
  cb <- uncurry encodeChannel (fromJust $ lookup Cb cis) $ R.map (\(_,cb,_) -> cb) ybr
  cr <- uncurry encodeChannel (fromJust $ lookup Cr cis) $ R.map (\(_,_,cr) -> cr) ybr
  return $ BakaImage (R.extent img) $ V.fromList [(Y, y), (Cb, cb), (Cr, cr)]
encodeImage cis (Grey img) = do
  y <- uncurry encodeChannel (fromJust $ lookup Y cis) img
  return $ BakaImage (R.extent img) [(Y, y)]
