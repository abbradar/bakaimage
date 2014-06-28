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

data BitBuf = BitBuf !Word8 !Int

newtype PutBit a = PutBit { unPutBit :: BitBuf -> PutM (BitBuf, a) }

instance Monad PutBit where
  return a = PutBit $ return . (, a)
  a >>= b = PutBit $ \p -> do
    (p', r) <- unPutBit a p
    unPutBit (b r) p'
  fail = PutBit . const . fail

instance Applicative PutBit where
  pure = return
  (<*>) = ap

instance Functor PutBit where
  fmap = liftM

putBitWord8 :: Int -> Word8 -> PutBit ()
putBitWord8 i w
  | i <= sz = PutBit $ \(BitBuf b s) -> do
    let (b1, s1) = (b .|. (w `shiftL` s), s + i)
    if s1 < 8
      then return $ (BitBuf b1 s1, ())
      else do
        putWord8 b1
        return (BitBuf (w `shiftR` (sz - s)) (s1 - sz), ())
  | otherwise = fail $ "putBitWord8: bit number should be <= " ++ show sz

  where sz = finiteBitSize w

flushPut :: PutBit ()
flushPut = PutBit $ \(BitBuf b s) -> do
  when (s /= 0) $ putWord8 b
  return (BitBuf 0 0, ())

runPutBit :: PutBit a -> PutM a
runPutBit a = do
  (_, _, r) <- runPutBit' $ do
    r <- a
    flushPut
    return r
  return r

runPutBit' :: PutBit a -> PutM (Word8, Int, a)
runPutBit' a = do
  (BitBuf b s, r) <- unPutBit a $ BitBuf 0 0
  return (b, s, r)

liftPut :: PutM a -> PutBit a
liftPut a = PutBit $ \res@(BitBuf b s) -> do
  if s == 0
    then do
      r <- a
      return (res, r)
    else do
      let (r, bs) = runPutM a
      b' <- foldM (pushWord s) b $ BL.unpack bs
      return (BitBuf b' s, r)

  where pushWord ps pb w = do
          putWord8 $ pb .|. (w `shiftL` ps)
          return $ w `shiftR` (finiteBitSize pb - ps)

newtype GetBit a = GetBit { unGetBit :: BitBuf -> Get (BitBuf, a) }

instance Monad GetBit where
  return a = GetBit $ \p -> return (p, a)

  a >>= b = GetBit $ \p -> do
    (p', r) <- unGetBit a p
    unGetBit (b r) p'

  fail = GetBit . const . fail

instance Applicative GetBit where
  pure = return
  (<*>) = ap

instance Functor GetBit where
  fmap = liftM

getBitWord8 :: Int -> GetBit Word8
getBitWord8 n
  | n <= sz = GetBit $ \(BitBuf w s) -> do
    let nt = min n s
        r' = maskBits nt w
    if n <= s
      then return (BitBuf (w `shiftR` n) (s - n), r')
      else do
        w' <- getWord8
        let n' = n - s
        return (BitBuf (w' `shiftR` n') (sz - n'),
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
runGetBit' a = do
  (BitBuf b s, r) <- unGetBit a $ BitBuf 0 0
  return (b, s, r)

flushGet :: GetBit ()
flushGet = GetBit $ const $ return (BitBuf 0 0, ())

liftGet :: Get a -> GetBit a
liftGet a = GetBit $ const $ a >>= return . (BitBuf 0 0, )

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
