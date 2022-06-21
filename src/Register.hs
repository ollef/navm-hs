{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Register where

import Control.Monad.Reader
import Control.Monad.ST
import Data.Hashable
import Data.STRef

newtype Virtual = Virtual Int
  deriving (Eq, Show, Hashable, Enum)

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

runVirtualSupply :: VirtualSupply a -> a
runVirtualSupply (VirtualSupply s) = runST $ do
  ref <- newSTRef $ Virtual 0
  runReaderT s ref
