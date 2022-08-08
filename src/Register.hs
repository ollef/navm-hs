{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Register where

import Control.Monad.Reader
import Control.Monad.ST
import Data.BitSet (BitSet)
import qualified Data.BitSet as BitSet
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import Data.Hashable
import Data.Kind (Type)
import Data.STRef

newtype Virtual = V Int
  deriving (Eq, Ord, Show, Hashable, Enum)

newtype VirtualSupply a
  = VirtualSupply (forall s. ReaderT (STRef s Virtual) (ST s) a)
  deriving (Functor)

instance Applicative VirtualSupply where
  pure a = VirtualSupply $ pure a
  (<*>) = ap

instance Monad VirtualSupply where
  VirtualSupply s >>= f =
    VirtualSupply $ do
      a <- s
      let VirtualSupply s' = f a
      s'

fresh :: VirtualSupply Virtual
fresh = VirtualSupply $ do
  ref <- ask
  lift $ do
    next <- readSTRef ref
    let !next' = succ next
    writeSTRef ref next'
    pure next

runVirtualSupply :: Virtual -> VirtualSupply a -> a
runVirtualSupply v (VirtualSupply s) = runST $ do
  ref <- newSTRef v
  runReaderT s ref

type family RegisterType a :: Type

class FromRegister a where
  fromRegister :: RegisterType a -> a

type instance RegisterType Virtual = Virtual

instance FromRegister Virtual where
  fromRegister = id

data VirtualOr physical
  = Physical !physical
  | Virtual !Virtual
  deriving (Eq)

printVirtual :: Virtual -> Builder
printVirtual (V v) = "%" <> Builder.intDec v

type instance RegisterType (BitSet a) = RegisterType a

instance (FromRegister a, Enum a) => FromRegister (BitSet a) where
  fromRegister = BitSet.singleton . fromRegister
