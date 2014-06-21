import Control.Monad (unless)
import Control.Applicative ((<$>))
import qualified Data.Array.Repa.IO.DevIL as D
import qualified Data.ByteString.Lazy as BL

import Codec.Image.BakaImage.DevIL
import Codec.Image.BakaImage.Encoding
import Codec.Image.BakaImage.Decoding
import Codec.Image.BakaImage.Serialize
import Graphics.Process.Types

main :: IO ()
main = do
  enc <- do
    putStrLn "Reading image"
    img <- fromDevIL <$> D.runIL (D.readImage "/tmp/test.png")
    --let img = Grey $ R.delay $ R.fromListUnboxed (R.ix2 200 200) $ take (200*200) $ cycle [1..]
    putStrLn "Encoding image"
    enc <- encodeImage standardSettings img
    putStrLn "Writing image"
    BL.writeFile "/tmp/output.baka" $ writeImage enc
    return enc

  do
    putStrLn "Reading encoded image"
    enc' <- readImage <$> BL.readFile "/tmp/output.baka"
    --let enc' = enc
    unless (enc' == enc) $ fail "Encoded images differ"
    putStrLn "Decoding image"
    dec <- computeImageP $ decodeImage enc'
    putStrLn "Writing image"
    D.runIL $ D.writeImage "/tmp/output.png" $ toDevIL $ delayImage dec
