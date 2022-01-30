{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Offset where

newtype Offset = Offset Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral, Bounded)

data Flexible = Flexible !Offset !Offset
  deriving (Eq, Show)

offset :: Offset -> Flexible -> Flexible
offset o (Flexible o1 o2) = Flexible (o <> o1) (o <> o2)

rigid :: Flexible -> Maybe Offset
rigid (Flexible o1 o2)
  | o1 == o2 = Just o1
  | otherwise = Nothing

choice :: Flexible -> Flexible -> Flexible
choice (Flexible min1 max1) (Flexible min2 max2) = Flexible (min min1 min2) (max max1 max2)

instance Semigroup Flexible where
  Flexible o1 o1' <> Flexible o2 o2' = Flexible (o1 <> o2) (o1' <> o2')

instance Monoid Flexible where
  mempty = Flexible mempty mempty

instance Semigroup Offset where
  (<>) = (+)

instance Monoid Offset where
  mempty = 0
