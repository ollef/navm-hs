{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Relative.Tsil (
  Tsil,
  pattern Empty,
  pattern (:>),
  Relative.Tsil.map,
  Relative.Tsil.foldMap,
  Relative.Tsil.traverse,
) where

import qualified Data.Tsil as Data
import GHC.Exts
import Relative

toListWithOffset :: Relative a => Offset -> Data.Tsil a -> [a]
toListWithOffset o = go []
  where
    go acc Data.Empty = acc
    go acc (as Data.:> a) = go (offset o a : acc) as

data Tsil a = Tsil !Offset (Data.Tsil a)

pattern Empty :: Tsil a
pattern Empty <-
  Tsil _ Data.Empty
  where
    Empty = Tsil 0 mempty

pattern (:>) :: Relative a => Tsil a -> a -> Tsil a
pattern as :> a <-
  Tsil o ((Tsil o -> as) Data.:> (offset o -> a))
  where
    Tsil o as :> a = Tsil o (as Data.:> offset (- o) a)

map :: Relative a => (a -> b) -> Tsil a -> Tsil b
map f (Tsil o as) = Tsil 0 (fmap (f . offset o) as)

foldMap :: (Relative a, Monoid m) => (a -> m) -> Tsil a -> m
foldMap f (Tsil o as) = Prelude.foldMap (f . offset o) as

traverse :: (Relative a, Applicative f) => (a -> f b) -> Tsil a -> f (Tsil b)
traverse f (Tsil o as) = Tsil 0 <$> Prelude.traverse (f . offset o) as

instance (Show a, Relative a) => Show (Tsil a) where
  show = show . toList

instance Relative (Tsil a) where
  offset o (Tsil o' as) = Tsil (o + o') as

instance Relative a => Semigroup (Tsil a) where
  Tsil o1 as1 <> Tsil o2 as2 = Tsil o1 $ as1 <> fmap (offset $ o2 - o1) as2

instance Relative a => Monoid (Tsil a) where
  mempty = Empty

instance Relative a => IsList (Tsil a) where
  type Item (Tsil a) = a
  fromList = Tsil 0 . fromList
  toList (Tsil o as) = toListWithOffset o as

{-# COMPLETE Empty, (:>) #-}
