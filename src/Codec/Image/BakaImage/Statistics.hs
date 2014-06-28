{-# LANGUAGE LambdaCase #-}

module Codec.Image.BakaImage.Statistics where

import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Array.Repa (Z(..), (:.)(..))
import qualified Data.Array.Repa as R

import Codec.Image.BakaImage.Format
import Codec.Image.BakaImage.Blocks

statisticsChannel :: Size -> Channel -> [(BlockInfo, Float)]
statisticsChannel sz (Channel bis bs) = r
  where count (Block bii _ _) (_, (Z :. h :. w)) = (bii, h*ph + w*pw)
          where bi = bis V.! fromIntegral bii
                (Z :. ph :. pw) = pixel bi
        unify (bii, v) m = M.alter (\case Nothing -> Just v
                                          Just v' -> Just $ v + v') bii m
        ra = M.toList $ foldr unify M.empty $ R.toList $
             R.zipWith count bs $ blockSizes sz $ sbsize $ V.head bis
        r = map (\(bii, n) -> (bis V.! fromIntegral bii, fromIntegral n / s)) ra
        s = fromIntegral $ sum $ map snd ra

statisticsImage :: BakaImage -> [(ChannelType, [(BlockInfo, Float)])]
statisticsImage (BakaImage sz cs) = map (\(t, x) -> (t, statisticsChannel sz x)) $ V.toList cs
