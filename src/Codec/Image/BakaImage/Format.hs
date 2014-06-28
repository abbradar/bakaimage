 {-# LANGUAGE DeriveGeneric #-}

module Codec.Image.BakaImage.Format where

import Data.Word (Word8)
import qualified Data.Array.Repa as R
import Data.Vector (Vector)
import Data.Array.Repa (Array, DIM2)
import qualified Data.Array.Repa.Repr.Vector as RV
import GHC.Generics (Generic)

import Graphics.Process.Types

type Size = DIM2

data ChannelType = Y | Cb | Cr
             deriving (Show, Eq, Generic)

data BakaImage = BakaImage Size (Vector (ChannelType, Channel))
               deriving (Show, Eq)

data Channel = Channel (Vector BlockInfo) (Array RV.V DIM2 Block)
             deriving (Show, Eq)

data BlockInfo = BlockInfo { levels :: Int
                           , sbsize :: Size
                           , pixel :: Size
                           , bitrate :: Int
                           }
               deriving (Show, Eq, Generic)

type Level = Array R.U DIM2 Word8

data Block = Block Word8 (Vector Level) (Image R.U)
           deriving (Show, Eq)
