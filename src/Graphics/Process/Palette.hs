module Graphics.Process.Palette where

import Data.Function (on)
import Data.List (minimumBy, partition)

import Math

{- Simple palette-building for single dimension using recursive mid value -}
paletteMid :: (Ord a, Num a, Integral a) => Int -> [a] -> [a]
paletteMid 0 _ = []
paletteMid 1 xs = [midD' xs]
paletteMid n xs = if length xs1 == 0
                  then take n $ repeat m
                  else paletteMid n' xs1 ++ paletteMid (n - n') xs2
  where m = midD' xs
        (xs1, xs2) = partition (< m) xs
        n' = n `div` 2

{- K-Means-based palette-building -}
paletteK :: (Ord a, Num a, Integral a) => Int -> [a] -> [a]
paletteK n ps = map fst $ kmeans as1 cl1
  where (as1, cl1) =
          let as = assign (paletteMid n ps) ps
              up = update as
          in (as, up)
        
        assign cl = map (\p -> (minimumBy (compare `on` dist (fromIntegral p)) cl, p))

        update [] = []
        update ((c, p):xs) = midD' (p:map snd xt) : update xt'
          where (xt, xt') = partition ((== c) . fst) xs

        kmeans as cl = if as == as' then as' else kmeans as' up'
          where as' = assign cl ps
                up' = update as
