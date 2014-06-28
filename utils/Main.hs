{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

import System.Console.CmdLib (RecordCommand(..),
                              dispatch, recordCommands,
                              (%>), Attribute(..),
                              Attributes(..), group)
import Data.Conduit (($$))
import Data.Conduit.Binary (sourceFile, sinkFile)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Vector as V
import qualified Data.Array.Repa as R
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<$>))
import System.Environment (getArgs)
import qualified Data.Array.Repa.IO.DevIL as D
import Data.Typeable (Typeable)
import Data.Data (Data)
import System.IO (hSetBuffering, stdin, stdout, BufferMode(..))

import qualified Codec.Image.BakaImage.Encoding as E
import qualified Codec.Image.BakaImage.Format as F
import Codec.Image.BakaImage.DevIL
import Codec.Image.BakaImage.Decoding
import Codec.Image.BakaImage.Serialize
import Codec.Image.BakaImage.Statistics
import Codec.Image.BakaImage.Dithering
import Graphics.Process.Types
import Graphics.Process.Palette

data BlockSettings = BlockSettings { levels :: Int
                                   , sbsize :: (Int, Int)
                                   , pixel :: (Int, Int)
                                   , bitrate :: Int
                                   }
                   deriving (Show, Eq, Typeable, Data)

lumaChannel :: [BlockSettings]
lumaChannel = [ BlockSettings { levels = 2
                              , sbsize = (12, 12)
                              , pixel = (2, 2)
                              , bitrate = 8
                              }
              , BlockSettings { levels = 4
                              , sbsize = (12, 12)
                              , pixel = (2, 2)
                              , bitrate = 8
                              }
              , BlockSettings { levels = 4
                              , sbsize = (6, 6)
                              , pixel = (1, 1)
                              , bitrate = 8
                              }
              , BlockSettings { levels = 2
                              , sbsize = (3, 3)
                              , pixel = (1, 1)
                              , bitrate = 8
                              }
              ]
              
colorChannel :: [BlockSettings]
colorChannel = [ BlockSettings { levels = 2
                               , sbsize = (12, 12)
                               , pixel = (2, 2)
                               , bitrate = 6
                               }
               , BlockSettings { levels = 4
                               , sbsize = (12, 12)
                               , pixel = (2, 2)
                               , bitrate = 6
                               }
               , BlockSettings { levels = 4
                               , sbsize = (6, 6)
                               , pixel = (1, 1)
                               , bitrate = 6
                               }
               , BlockSettings { levels = 2
                               , sbsize = (3, 3)
                               , pixel = (1, 1)
                               , bitrate = 6
                               }
               ]

data ChannelSettings = ChannelSettings { palette :: String
                                       , threshold :: Float
                                       , errorK :: Float
                                       , dither :: String
                                       }
                       deriving (Show, Eq, Typeable, Data)

lumaEncoding :: ChannelSettings
lumaEncoding = ChannelSettings { palette = "kmeans"
                               , dither = "no"
                               , threshold = 20
                               , errorK = 1.2
                               }
               
colorEncoding :: ChannelSettings
colorEncoding = ChannelSettings { palette = "kmeans"
                                , dither = "no"
                                , threshold = 30
                                , errorK = 1.4
                                }

type EncodingSettings = [(String, (ChannelSettings, [BlockSettings]))]

standardSettings :: EncodingSettings
standardSettings = [ ("y", (lumaEncoding, lumaChannel))
                   , ("cb", (colorEncoding, colorChannel))
                   , ("cr", (colorEncoding, colorChannel))
                   ]

fromEncSettings :: Float -> Float -> EncodingSettings -> E.Encoding
fromEncSettings myK myT = map (\(n, (v, bi)) -> (fromName n, (fromChannel n v, V.fromList $ map fromBlock bi)))
  where fromChannel n s = E.EncodingInfo { E.palette = fromPalette $ palette s
                                         , E.threshold = if n == "y"  then myT else myT * 1.5
                                         , E.errorK = if n == "y" then myK else myK * (1.4/1.2)
                                         , E.dither = fromDither $ dither s
                                         }

        fromName "y" = F.Y
        fromName "cb" = F.Cb
        fromName "cr" = F.Cr
        fromName _ = error "Unknown channel name"

        fromBlock bi@(BlockSettings { sbsize = (sw, sh)
                                    , pixel = (pw, ph)
                                    })
          = F.BlockInfo { F.levels = levels bi
                        , F.sbsize = R.ix2 sh sw
                        , F.pixel = R.ix2 ph pw
                        , F.bitrate = bitrate bi
                        }
        
        fromPalette "mid" = paletteMid
        fromPalette "kmeans" = paletteK
        fromPalette "2lvl" = palette2
        fromPalette _ = error "Unknown palette selection algorithm"

        fromDither "no" = noDither
        fromDither "ED" = ditherED
        fromDither _ = error "Unknown dithering algorithm"

data Mode = Encode { encoding :: EncodingSettings, setK :: Float, setThr :: Float, from :: String, to :: String }
          | Decode { from :: String, to :: String }
          | Diff { imageA :: String, imageB :: String }
          | Statistics { image :: String }
          deriving (Data, Typeable, Eq, Show)

instance RecordCommand Mode where
  mode_summary (Encode {}) = "BakaImage image encoder"
  mode_summary (Decode {}) = "BakaImage image decoder"
  mode_summary (Diff {}) = "Error between two images"
  mode_summary (Statistics {}) = "Image statistics"

  rec_options (Encode {}) = group "Encoding options"
                            [ encoding %> [ Help "Encoding", Default standardSettings ]
                            , setK %> [ Help "Tolerated error K", Default (1.2 :: Float) ]
                            , setThr %> [ Help "Error threshold", Default (20 :: Float) ]
                            , from %> [ Help "Input file", Required True, Positional 0 ]
                            , to %> [ Help "Output file", Required True, Positional 1 ]
                            ]
  rec_options (Decode {}) = group "Decoding options"
                            [ from %> [ Help "Input file", Required True, Positional 0 ]
                            , to %> [ Help "Output file", Required True, Positional 1 ]
                            ]
  rec_options (Diff {}) = group "Difference"
                          [ imageA %> [ Help "First image", Required True, Positional 0 ]
                          , imageB %> [ Help "Second image", Required True, Positional 1 ]
                          ]
  rec_options (Statistics {}) = group "Statistics"
                                [ image %> [ Help "Encoded image", Required True, Positional 0 ]
                                ]

                            
  run' cmd@(Encode {}) _ = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    putStr "K = "
    myK <- read <$> getLine
    putStr "Thr = "
    myThr <- read <$> getLine
    img <- fromDevIL <$> (D.runIL $ D.readImage $ from cmd)
    enc <- E.encodeImage (fromEncSettings myK myThr $ encoding cmd) img
    runResourceT $ writeImage enc $$ sinkFile (to cmd)

  run' cmd@(Decode {}) _ = do
    enc' <- runResourceT $ sourceFile (from cmd) $$ readImage
    dec <- computeImageP $ decodeImage enc'
    D.runIL $ D.writeImage (to cmd) $ toDevIL $ delayImage dec

  run' cmd@(Diff {}) _ = D.runIL $ do
    (Grey imgA) <- fromDevIL <$> (D.readImage $ imageA cmd)
    (Grey imgB) <- fromDevIL <$> (D.readImage $ imageB cmd)
    liftIO $ mapM_ (putStrLn . show) $ E.diff imgA imgB

  run' cmd@(Statistics {}) _ = do
    enc <- runResourceT $ sourceFile (image cmd) $$ readImage
    putStrLn $ "Statistics:\n" ++ show (statisticsImage enc)

instance Attributes Mode where

main :: IO ()
main = getArgs >>= dispatch [] (recordCommands (undefined :: Mode))
