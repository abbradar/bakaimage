module Codec.Image.BakaImage.Dithering where

import Control.Monad (forM_, when)
import Control.Applicative ((<$>))
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VM
import Control.Monad.ST (runST)
import Data.Array.Repa (Z(..), (:.)(..))
import Data.Array.Repa.Repr.Unboxed as RU
import Data.Array.Repa.Repr.Vector as RV
import qualified Data.Array.Repa as R

import Math
import Graphics.Process.Types
import Codec.Image.BakaImage.Format
import Codec.Image.BakaImage.Size
import Codec.Image.BakaImage.Packing
import Codec.Image.BakaImage.Decoding

noDither :: Image R.D -> Channel -> Channel
noDither _ = id

{- Error diffusion dithering based on "Color quantization using octrees" paper -}
ditherED :: Image R.D -> Channel -> Channel
ditherED img eimg = runST $ do
  let lvl = RV.computeVectorS $ R.map (map fromIntegral . fst) $ flattenChannel (fromIx2 sz) eimg
      v = RU.toUnboxed $ RU.computeUnboxedS $ R.map fromIntegral img
  mv <- VU.unsafeThaw v
  nv <- VM.new $ w * h
  forM_ [0..w-1] $ \x -> do
    forM_ [0..h-1] $ \y -> do
      let i = ix x y
      p <- VM.read mv i
      let (ip, p') = quantize (lvl R.! R.ix2 y x) p
      VM.write nv i ip
      let err = p - p' :: Float
          err38 = err * (3/8)
      when (x < w - 1) $ update (+ err38) mv $ ix (x+1) y
      when (y < h - 1) $ update (+ err38) mv $ ix x (y+1)
      when ((x < w - 1) && (y < h - 1)) $ update (+ (err / 4)) mv $ ix (x+1) (y+1)
  v' <- VU.unsafeFreeze nv
  return $ packChannel eimg $ R.delay $ RU.fromUnboxed sz v'

  where ix x y = y * w + x
        update f m i = f <$> VM.read m i >>= VM.write m i
        sz@(Z :. h :. w) = R.extent img
