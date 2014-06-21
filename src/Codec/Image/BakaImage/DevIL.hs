module Codec.Image.BakaImage.DevIL where

import Data.Array.Repa (Z(..), (:.)(..),
                        ix3)
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.IO.DevIL as D
import Graphics.Process.Types

fromDevIL :: D.Image -> MImage R.D
fromDevIL (D.RGBA _) = error "Alpha channel is not supported"
fromDevIL (D.BGRA _) = error "Alpha channel is not supported"
fromDevIL (D.RGB img) = RGB $ R.fromFunction (R.ix2 h w)
                        (\(Z :. y :. x) -> (img R.! ix3 y x 0,
                                            img R.! ix3 y x 1,
                                            img R.! ix3 y x 2))
  where (Z :. h :. w :. 3) = R.extent img
fromDevIL (D.BGR img) = RGB $ R.fromFunction (R.ix2 h w)
                        (\(Z :. y :. x) -> (img R.! ix3 y x 2,
                                            img R.! ix3 y x 1,
                                            img R.! ix3 y x 0))
  where (Z :. h :. w :. 3) = R.extent img
fromDevIL (D.Grey img) = Grey $ R.delay img

toDevIL :: MImage R.D -> D.Image
toDevIL (RGB img) = D.RGB $ R.computeS $ R.fromFunction (R.ix3 h w 3)
                    (\(p@(Z :. _ :. _) :. c) -> let (r, g, b) = img R.! p
                                                in case c of
                                                  0 -> r
                                                  1 -> g
                                                  2 -> b
                                                  _ -> undefined)
  where (Z :. h :. w) = R.extent img
toDevIL (Grey img) = D.Grey $ R.copyS img
