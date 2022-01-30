{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

module ArrayBuilder where

import Control.Monad.ST
import Data.Foldable
import Data.Primitive.PrimArray
import Data.Primitive.Ptr
import Data.Semigroup
import Data.Word
import GHC.Exts hiding (build)
import GHC.ST
import Offset (Offset (Offset))
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
