{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

module ArrayBuilder where

import Control.Monad.ST
import Data.Foldable
import Data.Int
import Data.Primitive.PrimArray
import Data.Primitive.Ptr
import Data.Semigroup
import Data.Word
import Foreign.Ptr (castPtr)
import GHC.Exts hiding (build)
import GHC.ST
import Offset (Offset (Offset))
import qualified System.ByteOrder as ByteOrder
import Prelude

data ArrayBuilder s = ArrayBuilder
  { size :: !Offset
  , function :: !((# State# s, Addr# #) -> State# s)
  }

st :: Offset -> (Ptr Word8 -> ST s ()) -> ArrayBuilder s
st bytes f =
  ArrayBuilder bytes $ \(# s, addr #) -> do
    let (ST inner) = f (Ptr addr)
    let !(# s', () #) = inner s
    s'

run :: (forall s. ArrayBuilder s) -> PrimArray Word8
run builder =
  runST $ do
    let ArrayBuilder {size, function} = builder
    arr <- newPinnedPrimArray $ fromIntegral size
    let !(Ptr startAddr) = mutablePrimArrayContents arr
    ST
      ( \s -> do
          let !s' = function (# s, startAddr #)
          (# s', () #)
      )
    unsafeFreezePrimArray arr

instance Semigroup (ArrayBuilder m) where
  ArrayBuilder size1 f <> ArrayBuilder size2 g =
    ArrayBuilder
      (size1 + size2)
      ( \x@(# _, addr #) -> do
          let !s = f x
              !(Ptr addr') = advancePtr (Ptr addr :: Ptr Word8) (coerce size1)
          g (# s, addr' #)
      )

instance Monoid (ArrayBuilder m) where
  mempty = ArrayBuilder 0 (\(# s, _ #) -> s)
  mconcat bs =
    ArrayBuilder
      ( foldl'
          (\n (ArrayBuilder size _) -> n + size)
          0
          bs
      )
      ( snd $
          foldl'
            ( \(!size1, !f) (ArrayBuilder size2 g) ->
                ( size1 + size2
                , \x@(# _, addr #) -> do
                    let !s = f x
                        !(Ptr addr') = advancePtr (Ptr addr :: Ptr Word8) (coerce size1)
                    g (# s, addr' #)
                )
            )
            (0, \(# s, _ #) -> s)
            bs
      )

skip :: Offset -> ArrayBuilder s
skip size = ArrayBuilder (coerce size) $ \(# s, _addr #) -> s

overlay :: ArrayBuilder s -> ArrayBuilder s -> ArrayBuilder s
overlay (ArrayBuilder size1 function1) (ArrayBuilder size2 function2) =
  ArrayBuilder (max size1 size2) $ \(# s, addr #) -> do
    let !s' = function1 (# s, addr #)
    function2 (# s', addr #)

overlays :: [ArrayBuilder s] -> ArrayBuilder s
overlays builders =
  ArrayBuilder
    (foldl' (\n (ArrayBuilder size _) -> max n size) 0 builders)
    (go builders)
  where
    go [] (# s, _ #) = s
    go (ArrayBuilder _ function : builders') arg@(# _, addr #) = do
      let !s' = function arg
      go builders' (# s', addr #)

word8 :: Word8 -> ArrayBuilder s
word8 w = st 1 $ \ptr -> writeOffPtr ptr 0 w

word16 :: Word16 -> ArrayBuilder s
word16 w = st 2 $ \ptr -> writeOffPtr (castPtr ptr) 0 $ ByteOrder.toLittleEndian w

word32 :: Word32 -> ArrayBuilder s
word32 w = st 4 $ \ptr -> writeOffPtr (castPtr ptr) 0 $ ByteOrder.toLittleEndian w

word64 :: Word64 -> ArrayBuilder s
word64 w = st 8 $ \ptr -> writeOffPtr (castPtr ptr) 0 $ ByteOrder.toLittleEndian w

int8 :: Int8 -> ArrayBuilder s
int8 = word8 . fromIntegral

int16 :: Int16 -> ArrayBuilder s
int16 = word16 . fromIntegral

int32 :: Int32 -> ArrayBuilder s
int32 = word32 . fromIntegral

int64 :: Int64 -> ArrayBuilder s
int64 = word64 . fromIntegral
