module Codec.Image.BakaImage.Serialize where

import Control.Applicative ((<$>))
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import Data.Bits (shiftL, shiftR, finiteBitSize)
import Data.Array.Repa (Z(..), (:.)(..))
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.Vector as RV
import Control.Monad (forM, forM_)
import Data.Binary (Binary(..), Put, Get)
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)

import Codec.Image.BakaImage.Format
import Codec.Image.BakaImage.Size
import Codec.Image.BakaImage.Blocks
import Data.Binary.Bit
import Graphics.Process.Types
import Math

instance Binary ChannelType
instance Binary BlockInfo

putImageBitrate :: Int -> Image R.U -> PutBit ()
putImageBitrate b img = forM_ (map (\x -> x `shiftR` (finiteBitSize x - b)) $ R.toList $ img) $ putBitWord8 b

putBlock :: Vector BlockInfo -> Block -> PutBit ()
putBlock bis (Block bii lvls img) = do
  let bi = bis V.! fromIntegral bii
  putBitWord8 (numBitSize $ V.length bis) bii
  mapM_ (putImageBitrate $ fromIntegral $ bitrate bi) $ V.toList lvls
  putImageBitrate (numBitSize $ levels bi) img

putChannel :: Channel -> Put
putChannel (Channel bis blocks) = do
  put $ V.toList bis
  runPutBit $ forM_ (R.toList blocks) (putBlock bis)

putImage :: BakaImage -> Put
putImage (BakaImage s cs) = do
  put s
  put $ V.toList $ V.map fst cs
  mapM_ (putChannel . snd) $ V.toList cs

getImageBitrate :: Size -> Int -> GetBit (Image R.U)
getImageBitrate s@(w, h) b = do
  pts <- forM [1..w*h] $ const $ getBitWord8 b
  return $ R.fromListUnboxed (toIx2 s) $ map (\x -> x `shiftL` (finiteBitSize x - b)) pts

getBlock :: Vector BlockInfo -> Size -> GetBit Block
getBlock bis sz = do
  bii <- getBitWord8 (numBitSize $ V.length bis)
  let bi = bis V.! fromIntegral bii
      bsz = R.extent $ blockSizes sz $ sbsize bi
      psz = R.extent $ blockSizes sz $ pixel bi
  lvls <- forM [1..levels bi] $ const $ getImageBitrate (fromIx2 bsz) (bitrate bi)
  img <- getImageBitrate (fromIx2 psz) (numBitSize $ levels bi)
  return $ Block bii (V.fromList lvls) img

getChannel :: Size -> Get Channel
getChannel sz = do
  bis <- V.fromList <$> get
  let bsz@(Z :. bh :. bw) = R.extent $ blockSizes sz $ sbsize $ V.head bis
  bs <- runGetBit $ forM [1..bh*bw] $
        const $ getBlock bis $ fromIx2 bsz
  return $ Channel bis (RV.fromListVector bsz bs)

getImage :: Get BakaImage
getImage = do
  sz <- get
  cts <- V.fromList <$> get
  cs <- forM [1..V.length cts] $ const $ getChannel sz
  return $ BakaImage sz $ V.zip cts (V.fromList cs)

writeImage :: BakaImage -> BL.ByteString
writeImage = runPut . putImage

readImage :: BL.ByteString -> BakaImage
readImage = runGet getImage
