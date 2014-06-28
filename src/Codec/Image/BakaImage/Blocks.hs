{-# LANGUAGE TemplateHaskell #-}

module Codec.Image.BakaImage.Blocks where

import qualified Data.Array.Repa as R
import Data.Array.Repa (Array, DIM2, Z(..), (:.)(..))
import qualified Data.Array.Repa.Repr.Vector as RV
import Test.QuickCheck (Property, Gen,
                        Positive(..),
                        forAll, choose,
                        arbitrary, quickCheckAll)

import Codec.Image.BakaImage.Format
import Math

blockify :: Size -> Array R.D DIM2 e -> Array R.D DIM2 (Array R.D DIM2 e)
blockify bs arr = R.map (\(p, s) -> R.extract p s arr) $ blockSizes (R.extent arr) bs

blockSizes :: Size -> Size -> Array R.D DIM2 (Size, Size)
blockSizes (Z :. h :. w) (Z :. bh :. bw)
  | bw > 0 && bh > 0 = R.fromFunction (R.ix2 (h `div1` bh) (w `div1` bw))
                                       (\(Z :. by :. bx) ->
                                         let sx = bx * bw
                                             sy = by * bh
                                         in (R.ix2 sy sx, R.ix2 (min bh (h - sy)) (min bw (w - sx)))
                                       )
  | otherwise = error "blockSizes: block size must be bigger than (1, 1)"

testSize :: Gen Size
testSize = do
  Positive w <- arbitrary
  Positive h <- arbitrary
  return $ R.ix2 h w

testBSizes :: Gen (Size, Size)
testBSizes = do
  s@(Z :. h :. w) <- testSize
  bw <- choose (1, w)
  bh <- choose (1, h)
  return (s, R.ix2 bh bw)

testArray :: Size -> Array R.U DIM2 Int
testArray s@(Z :. h :. w) = R.fromListUnboxed s [1..w*h]

testBlocks :: Gen (Size, Size, Array R.U DIM2 Int)
testBlocks = do
  (s, bs) <- testBSizes
  return (s, bs, testArray s)

prop_blockSizesZeros :: Property
prop_blockSizesZeros = forAll testBSizes $
                       all (\(_, Z :. h :. w) -> w /= 0 && h /= 0) . R.toList . uncurry blockSizes

prop_blockSizesRange :: Property
prop_blockSizesRange = forAll testBSizes $ \(s@(Z :. h :. w), bs) ->
                       all (\(Z :. y :. x, Z :. bh :. bw) -> x+bw <= w && y+bh <= h) $ R.toList $ blockSizes s bs

deblockifyBy :: ((Size, Size) -> a -> Array R.D DIM2 e) -> Size -> Size -> Array R.D DIM2 a -> Array R.D DIM2 e
deblockifyBy f s bs@(Z :. bh :. bw) arr' = R.fromFunction
                                    s
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
      img' = R.computeUnboxedS $ imgs R.! R.ix2 0 0
  in R.extent imgs == R.ix2 1 1 && img == img'

return []
runTests :: IO Bool
runTests = $quickCheckAll
