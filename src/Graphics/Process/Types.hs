module Graphics.Process.Types where

import Control.Applicative ((<$>))
import Data.Word (Word8)
import qualified Data.Array.Repa as R
import Data.Array.Repa (Array, DIM2)

type Image r = Array r DIM2 Word8
type RGBImage r = Array r DIM2 (Word8, Word8, Word8)

data MImage r = RGB !(RGBImage r)
              | Grey !(Image r)

computeImageS :: MImage R.D -> MImage R.U
computeImageS (RGB img) = RGB $ R.computeUnboxedS img
computeImageS (Grey img) = Grey $ R.computeUnboxedS img

computeImageP :: MImage R.D -> IO (MImage R.U)
computeImageP (RGB img) = RGB <$> R.computeUnboxedP img
computeImageP (Grey img) = Grey <$> R.computeUnboxedP img

delayImage :: MImage R.U -> MImage R.D
delayImage (RGB img) = RGB $ R.delay img
delayImage (Grey img) = Grey $ R.delay img
