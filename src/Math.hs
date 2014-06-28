{-# LANGUAGE ScopedTypeVariables #-}

module Math where

import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Bits (Bits, shiftR)

mid :: (Num a, Floating a) => [a] -> a
mid [] = error "mid: empty list"
mid x = sum x / fromIntegral (length x)

midD :: (Num a, Integral a) => [a] -> a
midD [] = error "midD: empty list"
midD x = sum x `div` fromIntegral (length x)

quantize :: (Enum a, Ord a, Num a, Num b, Enum b) => [a] -> a -> (b, a)
quantize cl p = minimumBy (comparing $ dist p . snd) $ zip [0..] cl
 
dist :: (Num a) => a -> a -> a
dist a b = abs $ a - b

div1 :: (Integral a) => a -> a -> a
a `div1` b = q + if r > 0 then 1 else 0
  where (q, r) = a `quotRem` b

infixl 7 `div1`

trimRange :: (Ord a) => a -> a -> a -> a
trimRange low high a
  | a < low = low
  | a > high = high
  | otherwise = a

between :: (Ord a) => a -> a -> a -> Bool
between low high a = a >= low && a <= high

numBitSize :: (Bits a, Num a) => a -> Int
numBitSize 0 = 0
numBitSize n = succ $ numBitSize $ n `shiftR` 1

truncTrim :: forall a b. (Ord a, Num a, RealFrac a, Integral b, Bounded b) => a -> b
truncTrim = truncate . trimRange (fromIntegral (minBound :: b)) (fromIntegral (maxBound :: b))
