module Graphics.Process.Palette where

import Data.Function (on)
import Data.List (minimumBy, partition)

import Math

{- Simple palette-building for single dimension using recursive mid value -}
paletteMid :: (Ord a, Num a, Integral a) => Int -> [a] -> [a]
paletteMid 0 _ = []
paletteMid 1 xs = [midD xs]
paletteMid n xs = if length xs1 == 0
                  then take n $ repeat m
                  else paletteMid n' xs1 ++ paletteMid (n - n') xs2
  where m = midD xs
        (xs1, xs2) = partition (< m) xs
        n' = n `div` 2

{- K-Means-based palette-building -}
paletteK :: (Ord a, Num a, Integral a) => Int -> [a] -> [a]
paletteK n ps = take n $ cycle $ kmeans ps as1 cl1
  where as1 = kmeansAssign (paletteMid n ps) ps
        cl1 = kmeansUpdate as1

kmeansAssign :: (Ord a, Num a, Integral a) => [a] -> [a] -> [(a, a)]
kmeansAssign cl = map (\p -> (minimumBy (compare `on` dist p) cl, p))

kmeansUpdate :: (Ord a, Num a, Integral a) => [(a, a)] -> [a]
kmeansUpdate [] = []
kmeansUpdate ((c, p):xs) = midD (p:map snd xt) : kmeansUpdate xt'
  where (xt, xt') = partition ((== c) . fst) xs
        
kmeans :: (Ord a, Num a, Integral a) => [a] -> [(a, a)] -> [a] -> [a]
kmeans ps as cl = if as == as' then cl else kmeans ps as' up'
  where as' = kmeansAssign cl ps
        up' = kmeansUpdate as'

palette2 :: (Ord a, Num a, Integral a, Show a) => Int -> [a] -> [a]
palette2 1 l = [midD l]
palette2 2 l = [foldr1 min l, foldr1 max l]
palette2 n xs = if length xs1 == 0
                  then take n $ repeat m
                  else palette2 n' xs1 ++ palette2 (n - n') xs2
  where m = midD xs
        (xs1, xs2) = partition (< m) xs
        n' = n `div` 2
