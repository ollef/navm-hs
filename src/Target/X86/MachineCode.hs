{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Target.X86.MachineCode where

import Control.Applicative
import Control.Monad.ST
import Data.ByteString.Internal as ByteString.Internal
import Data.Foldable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Hashable
import Data.Primitive.PrimArray
import Data.Primitive.Ptr
import Data.Primitive.Types
import Data.Word
import GHC.Exts hiding (build)
import GHC.ST
import Relative
import qualified Relative.Tsil as Relative (Tsil)

{-# ANN module "HLint: ignore Use id" #-}

newtype MachineCode = MachineCode (PrimArray Word8)
  deriving (Show)

newtype Label = Label ByteString
  deriving (Eq, Show, Hashable)

newtype Size = Size Int
  deriving (Show, Eq, Ord, Num, Real, Enum, Integral)

instance Relative Size where
  offset _ s = s

data LabelState = LabelState
  { definition :: Maybe Offset
  , uses :: Relative.Tsil LabelUse
  }
  deriving (Show)

data LabelUse = LabelUse
  { position :: !Offset
  , size :: !Size
  }
  deriving (Show)

instance Relative LabelUse where
  offset o (LabelUse p s) = LabelUse (offset o p) s

instance Relative LabelState where
  offset o (LabelState d u) = LabelState (offset o d) (offset o u)

instance Semigroup LabelState where
  LabelState d1 us1 <> LabelState d2 us2 = LabelState (d1 <|> d2) (us2 <> us1)

instance Monoid LabelState where
  mempty = LabelState mempty mempty

newtype Labels = Labels (HashMap Label LabelState)
  deriving (Show)

instance Semigroup Labels where
  Labels ls1 <> Labels ls2 = Labels $ HashMap.unionWith (<>) ls1 ls2

instance Monoid Labels where
  mempty = Labels mempty

instance Relative Labels where
  offset o (Labels ls) = Labels $ offset o <$> ls

data Builder s = Builder
  { size :: !Size
  , labels :: Labels
  , function :: !((# State# s, Addr# #) -> State# s)
  }

stBuilder ::
  Size ->
  (Ptr Word8 -> ST s ()) ->
  Builder s
stBuilder bytes f =
  Builder bytes mempty $ \(# s, addr #) -> do
    let (ST st) = f (Ptr addr)
    let !(# s', () #) = st s
    s'

define :: Label -> Builder s
define label =
  Builder
    { size = 0
    , labels =
        Labels $
          HashMap.singleton
            label
            LabelState
              { definition = Just 0
              , uses = mempty
              }
    , function = \(# s, _ #) -> s
    }

use :: forall a s. Prim a => Label -> Builder s
use label =
  Builder
    { size = 0
    , labels =
        Labels $
          HashMap.singleton
            label
            LabelState
              { definition = Nothing
              , uses = [LabelUse {position = 0, size = coerce $ sizeOf (undefined :: a)}]
              }
    , function = \(# s, _ #) -> s
    }

runBuilder :: (forall s. Builder s) -> (MachineCode, Labels)
runBuilder builder =
  runST $ do
    let Builder {size, labels, function} = builder
    arr <- newPinnedPrimArray $ fromIntegral size
    let !(Ptr startAddr) = mutablePrimArrayContents arr
    ST
      ( \s -> do
          let !s' = function (# s, startAddr #)
          (# s', () #)
      )
    frozenArr <- unsafeFreezePrimArray arr
    pure (MachineCode frozenArr, labels)

instance Semigroup (Builder m) where
  Builder size1 labels1 f <> Builder size2 labels2 g =
    Builder
      (size1 + size2)
      (labels1 <> offset (coerce size1) labels2)
      ( \x@(# _, addr #) -> do
          let !s = f x
              !(Ptr addr') = advancePtr (Ptr addr :: Ptr Word8) (coerce size1)
          g (# s, addr' #)
      )

instance Monoid (Builder m) where
  mempty = Builder 0 mempty (\(# s, _ #) -> s)
  mconcat bs =
    Builder
      ( foldl'
          (\n (Builder size _ _) -> n + size)
          0
          bs
      )
      ( snd $
          foldl'
            (\(!size1, !l1) (Builder size2 l2 _) -> (size1 + size2, l1 <> offset (coerce size1) l2))
            (0, mempty)
            bs
      )
      ( snd $
          foldl'
            ( \(!size1, !f) (Builder size2 _ g) ->
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
