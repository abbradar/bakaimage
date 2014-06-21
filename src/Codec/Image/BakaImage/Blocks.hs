{-# LANGUAGE TemplateHaskell #-}

module Codec.Image.BakaImage.Blocks where

import qualified Data.Array.Repa as R
import Data.Array.Repa (Array, DIM2, Z(..), (:.)(..))
import qualified Data.Array.Repa.Repr.Vector as RV
import Test.QuickCheck (Property, Gen,
                        Positive(..),
                        forAll, choose,
                        arbitrary, quickCheckAll)

import Math
import Codec.Image.BakaImage.Size

blockify :: Size -> Array R.D DIM2 e -> Array R.D DIM2 (Array R.D DIM2 e)
blockify bs arr = R.map (\(p, s) -> R.extract (toIx2 p) (toIx2 s) arr) $ blockSizes (fromIx2 $ R.extent arr) bs

blockSizes :: Size -> Size -> Array R.D DIM2 (Size, Size)
blockSizes (w, h) (bw, bh)
  | bw > 0 && bh > 0 = R.fromFunction (R.ix2 (h `div1` bh) (w `div1` bw))
                                       (\(Z :. by :. bx) ->
                                         let sx = bx * bw
                                             sy = by * bh
                                         in ((sx, sy), (min bw (w - sx), min bh (h - sy)))
                                       )
  | otherwise = error "blockSizes: block size must be bigger than (1, 1)"

testSize :: Gen Size
testSize = do
  Positive w <- arbitrary
  Positive h <- arbitrary
  return (w, h)

testBSizes :: Gen (Size, Size)
testBSizes = do
  s@(w, h) <- testSize
  bw <- choose (1, w)
  bh <- choose (1, h)
  return (s, (bw, bh))

testArray :: Size -> Array R.U DIM2 Int
testArray s@(w, h) = R.fromListUnboxed (toIx2 s) [1..w*h]

testBlocks :: Gen (Size, Size, Array R.U DIM2 Int)
testBlocks = do
  (s, bs) <- testBSizes
  return (s, bs, testArray s)

prop_blockSizesZeros :: Property
prop_blockSizesZeros = forAll testBSizes $
                       all (\(_, (w, h)) -> w /= 0 && h /= 0) . R.toList . uncurry blockSizes

prop_blockSizesRange :: Property
prop_blockSizesRange = forAll testBSizes $ \(s@(w, h), bs) ->
                       all (\((x, y), (bw, bh)) -> x+bw <= w && y+bh <= h) $ R.toList $ blockSizes s bs

deblockifyBy :: ((Size, Size) -> a -> Array R.D DIM2 e) -> Size -> Size -> Array R.D DIM2 a -> Array R.D DIM2 e
deblockifyBy f s bs@(bw, bh) arr' = R.fromFunction
                                    (toIx2 s)
                                    (\(Z :. y :. x) ->
                                      let (by, py) = y `quotRem` bh
                                          (bx, px) = x `quotRem` bw
                                      in (arr R.! (R.ix2 by bx)) R.! (R.ix2 py px))
  where arr = RV.computeVectorS $ R.zipWith f (blockSizes s bs) arr'

deblockify :: Size -> Size -> Array R.D DIM2 (Array R.D DIM2 e) -> Array R.D DIM2 e
deblockify = deblockifyBy $ flip const

prop_blockify :: Property
prop_blockify = forAll testBlocks $ \(s, bs, img) ->
  img == R.computeUnboxedS (deblockify s bs $ blockify bs $ R.delay img)

prop_blockifyId :: Property
prop_blockifyId = forAll testSize $ \s ->
  let img = testArray s
      imgs = blockify s $ R.delay img
      img' = R.computeUnboxedS $ imgs R.! (Z :. 0 :. 0)
  in R.extent imgs == (Z :. 1 :. 1) && img == img'

return []
runTests :: IO Bool
runTests = $quickCheckAll
