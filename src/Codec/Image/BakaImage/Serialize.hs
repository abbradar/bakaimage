{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Codec.Image.BakaImage.Serialize where

import Control.Applicative ((<$>))
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.ByteString as B
import Data.Bits (shiftL, shiftR, finiteBitSize)
import Data.Array.Repa (Z(..), (:.)(..))
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.Vector as RV
import Data.Binary (Binary(..), Put, Get)
import Data.Conduit (Producer, Consumer, (=$=))
import Data.Conduit.Lzma (compress, decompress)
import Data.Conduit.Serialization.Binary (sourcePut, sinkGet)
import Control.Monad.Trans.Resource (MonadResource)

import Codec.Image.BakaImage.Format
import Codec.Image.BakaImage.Blocks
import Data.Binary.Bit
import Graphics.Process.Types
import Math

instance Binary ChannelType
instance Binary BlockInfo

instance Binary Size where
  get = uncurry R.ix2 <$> get
  put (Z :. h :. w) = put (h, w)

putImageBitrate :: Int -> Image R.U -> PutBit ()
putImageBitrate b img = mapM_ (putBitWord8 b) $ map (\x -> x `shiftR` (finiteBitSize x - b)) $ R.toList img

getImageBitrate :: Int -> Size -> GetBit (Image R.U)
getImageBitrate b s@(Z :. h :. w) = do
  pts <- mapM (const $ getBitWord8 b) [1..w*h]
  return $ R.fromListUnboxed s $ map (\x -> x `shiftL` (finiteBitSize x - b)) pts

putBlock :: Vector BlockInfo -> Block -> PutBit ()
putBlock bis (Block bii lvls img) = do
  let bi = bis V.! fromIntegral bii
  putBitWord8 (numBitSize $ V.length bis) bii
  mapM_ (putImageBitrate $ bitrate bi) $ V.toList lvls
  mapM_ (putBitWord8 $ numBitSize $ levels bi) $ R.toList img

getBlock :: Vector BlockInfo -> Size -> GetBit Block
getBlock bis sz = do
  bii <- getBitWord8 (numBitSize $ V.length bis)
  let bi = bis V.! fromIntegral bii
      psz@(Z :. h :. w) = R.extent $ blockSizes sz $ pixel bi
      bsz = R.extent $ blockSizes psz $ sbsize bi
  lvls <- mapM (const $ getImageBitrate (bitrate bi) bsz) [1..levels bi]
  pts <- mapM (const $ getBitWord8 $ numBitSize $ levels bi) [1..w*h]
  return $ Block bii (V.fromList lvls) (R.fromListUnboxed psz pts)

putChannel :: Channel -> Put
putChannel (Channel bis blocks) = do
  put $ V.toList bis
  runPutBit $ mapM_ (putBlock bis) $ R.toList blocks

getChannel :: Size -> Get Channel
getChannel sz = do
  bis <- V.fromList <$> get
  let bss = R.map snd $ blockSizes sz $ sbsize $ V.head bis
  bs <- runGetBit $ mapM (getBlock bis) $ R.toList bss
  return $ Channel bis (RV.fromListVector (R.extent bss) bs)

instance Binary BakaImage where
  put (BakaImage sz cs) = do
    put sz
    put $ V.toList $ V.map fst cs
    mapM_ (putChannel . snd) $ V.toList cs

  get = do
    sz <- get
    cts <- get
    cs <- mapM (const $ getChannel sz) cts
    return $ BakaImage sz $ V.zip (V.fromList cts) (V.fromList cs)

writeImage :: MonadResource m => BakaImage -> Producer m B.ByteString
writeImage img = sourcePut (put img) =$= compress Nothing

readImage :: MonadResource m => Consumer B.ByteString m BakaImage
readImage = decompress Nothing =$= sinkGet get
