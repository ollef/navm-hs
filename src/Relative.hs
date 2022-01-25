{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Relative where

import Data.Bifunctor

newtype Offset = Offset Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

instance Semigroup Offset where
  (<>) = (+)

instance Monoid Offset where
  mempty = 0

class Relative a where
  offset :: Offset -> a -> a

instance Relative Offset where
  offset = (+)

instance (Relative a, Relative b) => Relative (a, b) where
  offset o = bimap (offset o) (offset o)

instance (Relative a, Relative b) => Relative (Either a b) where
  offset o = bimap (offset o) (offset o)

instance Relative a => Relative (Maybe a) where
  offset = fmap . offset
