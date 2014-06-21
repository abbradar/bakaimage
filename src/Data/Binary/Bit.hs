{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Data.Binary.Bit where

import Control.Applicative (Applicative(..), (<$>))
import Control.Monad (ap, liftM, foldM, forM_, forM, when)
import Data.Bits (shiftL, shiftR, (.|.), finiteBitSize)
import Data.Word (Word8)
import qualified Data.ByteString.Lazy as BL
import Data.Binary (Binary(..))
import Data.Binary.Get (Get, runGet, getWord8)
import Data.Binary.Put (PutM, runPutM, runPut,
                        putWord8)
import Test.QuickCheck (Property, Gen,
                        property,
                        forAll, choose, listOf,
                        arbitrary, quickCheckAll)

data PutBuf a = PutBuf !Word8 !Int a

newtype PutBit a = PutBit { unPutBit :: PutM (PutBuf a) }

instance Monad PutBit where
  return a = PutBit $ return $ PutBuf 0 0 a

  a >>= b = PutBit $ do
    (PutBuf pb1 ps1 r1) <- unPutBit a
    if ps1 == 0
      then unPutBit $ b r1
      else do
        let (PutBuf pb2 ps2 r2, bs) = runPutM $ unPutBit $ b r1
            sz = finiteBitSize pb1
        ps' <- foldM (pushWord ps1) pb1 $ BL.unpack bs
        let p''@(pb'', ps'') = (ps' .|. (pb2 `shiftL` ps1), ps1 + ps2)
        (pbr, psr) <- if ps'' < sz
                      then return p''
                      else do
                        putWord8 pb''
                        return (pb2 `shiftR` (sz - ps1), ps'' - sz)
        return $ PutBuf pbr psr r2
    
    where pushWord ps pb w = do
            putWord8 $ pb .|. (w `shiftL` ps)
            return $ w `shiftR` (finiteBitSize pb - ps)

  fail = PutBit . fail

instance Applicative PutBit where
  pure = return
  (<*>) = ap

instance Functor PutBit where
  fmap = liftM

putBitWord8 :: Int -> Word8 -> PutBit ()
putBitWord8 i w
  | i < sz = PutBit $ return $ PutBuf w i ()
  | i == sz = PutBit $ putWord8 w >> return (PutBuf 0 0 ())
  | otherwise = fail $ "putBitWord8: bit number should be <= " ++ show sz

  where sz = finiteBitSize w

flushPut :: PutBit a -> PutBit a
flushPut a = PutBit $ do
  (PutBuf pb s r) <- unPutBit a
  when (s /= 0) $ putWord8 pb
  return (PutBuf 0 0 r)

runPutBit :: PutBit a -> PutM a
runPutBit a = do
  (PutBuf _ _ r) <- unPutBit $ flushPut a
  return r

runPutBit' :: PutBit a -> PutM (Word8, Int, a)
runPutBit' a = do
  (PutBuf b s r) <- unPutBit a
  return (b, s, r)

liftPut :: PutM a -> PutBit a
liftPut a = PutBit $ a >>= return . (PutBuf 0 0)

newtype GetBit a = GetBit { unGetBit :: Word8 -> Int -> Get (Word8, Int, a) }

instance Monad GetBit where
  return a = GetBit $ \w s -> return (w, s, a)

  a >>= b = GetBit $ \w s -> do
    (w1, s1, r) <- unGetBit a w s
    unGetBit (b r) w1 s1

  fail e = GetBit $ \_ _ -> fail e

instance Applicative GetBit where
  pure = return
  (<*>) = ap

instance Functor GetBit where
  fmap = liftM

getBitWord8 :: Int -> GetBit Word8
getBitWord8 n
  | n <= sz = GetBit $ \w s -> do
    let nt = min n s
        r' = maskBits nt w
    if n <= s
      then return (w `shiftR` n, s - n, r')
      else do
        w' <- getWord8
        let n' = n - s
        return (w' `shiftR` n', sz - n',
                r' .|. maskBits n (w' `shiftL` s))

  | otherwise = fail $ "getBitWord8: bit number should be <= " ++ show sz
  where maskBits n' w = w `shiftL` ni `shiftR` ni
          where ni = sz - n'
        sz = finiteBitSize (undefined :: Word8)

runGetBit :: GetBit a -> Get a
runGetBit a = do
  (_, _, r) <- runGetBit' a
  return r

runGetBit' :: GetBit a -> Get (Word8, Int, a)
runGetBit' a = unGetBit a 0 0

flushGet :: GetBit ()
flushGet = GetBit $ \_ _ -> return (0, 0, ())

liftGet :: Get a -> GetBit a
liftGet a = flushGet >> GetBit (\_ _ -> a >>= return . (0, 0,))

testBits :: Gen [(Int, Word8)]
testBits = listOf $ do
  w <- arbitrary
  s <- choose (0, finiteBitSize w)
  let ds = finiteBitSize w - s
  return (s, w `shiftL` ds `shiftR` ds)

testEncode :: [(Int, Word8)] -> BL.ByteString
testEncode b = runPut $ runPutBit $ do
  liftPut $ put $ length b
  forM_ b $ \(s, w) -> do
    liftPut (put (fromIntegral s :: Word8))
    putBitWord8 s w

testDecode :: BL.ByteString -> [(Int, Word8)]
testDecode = runGet $ runGetBit $ do
  l <- liftGet $ get
  forM [(1::Int)..l] $ \_ -> do
    s <- fromIntegral <$> getBitWord8 (finiteBitSize (undefined :: Word8))
    w <- getBitWord8 s
    return (s, w)

prop_bit :: Property
prop_bit = forAll testBits $ \b -> b == testDecode (testEncode b)

prop_encodeId :: Property
prop_encodeId = forAll testBits $ \b -> enc b == enc ([(0,0)] ++ b ++ [(0,0)])
  where enc b = runPut $ runPutBit $ forM_ b $ uncurry putBitWord8

prop_encode8 :: Property
prop_encode8 = property $ \x -> run (liftPut $ putWord8 x) == run (putBitWord8 (finiteBitSize x) x)
  where run = runPut . runPutBit

return []
runTests :: IO Bool
runTests = $quickCheckAll
