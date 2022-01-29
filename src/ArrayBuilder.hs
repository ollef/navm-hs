{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import GHC.Exts hiding (build)
import GHC.ST
import Prelude hiding (max, min)

newtype Size = Size Int
  deriving (Show, Eq, Ord, Num, Real, Enum, Integral)

data ArrayBuilder s = ArrayBuilder
  { size :: !Size
  , function :: !((# State# s, Addr# #) -> State# s)
  }

st ::
  Size ->
  (Ptr Word8 -> ST s ()) ->
  ArrayBuilder s
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
