import Control.Monad (unless)
import Control.Applicative ((<$>))
import System.Exit (exitFailure)

import qualified Codec.Image.BakaImage.Blocks (runTests)
import qualified Data.Binary.Bit (runTests)

tests :: [IO Bool]
tests = [ Codec.Image.BakaImage.Blocks.runTests
        , Data.Binary.Bit.runTests
        ]

main :: IO ()
main = do
  result <- all id <$> mapM id tests
  unless result exitFailure
