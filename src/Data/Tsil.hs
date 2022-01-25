{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Tsil where

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Foldable as Foldable
import Data.Hashable
import qualified GHC.Exts
import GHC.Generics

data Tsil a
  = Empty
  | Tsil a :> a
  deriving (Eq, Functor, Ord, Traversable, Generic, Hashable)

instance Show a => Show (Tsil a) where
  show = show . Foldable.toList

instance Semigroup (Tsil a) where
  xs <> Empty = xs
  xs <> (ys :> y) = (xs <> ys) :> y

instance Monoid (Tsil a) where
  mempty = Empty
  mappend = (<>)

instance Applicative Tsil where
  pure = (Empty :>)
  (<*>) = ap

instance Alternative Tsil where
  empty = Empty
  (<|>) = mappend

instance Monad Tsil where
  return = pure
  Empty >>= _ = Empty
  xs :> x >>= f = (xs >>= f) <> f x

instance GHC.Exts.IsList (Tsil a) where
  type Item (Tsil a) = a
  fromList = go Empty
    where
      go acc [] = acc
      go acc (a : as) = go (acc :> a) as
  toList = Foldable.toList

instance Foldable Tsil where
  foldMap _ Empty = mempty
  foldMap f (xs :> x) = foldMap f xs `mappend` f x
  toList = go []
    where
      go acc Empty = acc
      go acc (xs :> x) = go (x : acc) xs

null :: Tsil a -> Bool
null Empty = True
null (_ :> _) = False

lookup :: Eq a => a -> Tsil (a, b) -> Maybe b
lookup _ Empty = Nothing
lookup a (as :> (a', b))
  | a == a' = Just b
  | otherwise = Data.Tsil.lookup a as

filter :: (a -> Bool) -> Tsil a -> Tsil a
filter _ Empty = Empty
filter f (xs :> x)
  | f x = Data.Tsil.filter f xs :> x
  | otherwise = Data.Tsil.filter f xs

partition :: (a -> Bool) -> Tsil a -> (Tsil a, Tsil a)
partition _ Empty = mempty
partition p (xs :> x)
  | p x = first (:> x) $ Data.Tsil.partition p xs
  | otherwise = second (:> x) $ Data.Tsil.partition p xs

span :: (a -> Bool) -> Tsil a -> (Tsil a, Tsil a)
span _ Empty = (Empty, Empty)
span p as@(as' :> a)
  | p a = second (:> a) $ Data.Tsil.span p as'
  | otherwise = (as, Empty)

zip :: Tsil a -> Tsil b -> Tsil (a, b)
zip = Data.Tsil.zipWith (,)

zipWith :: (a -> b -> c) -> Tsil a -> Tsil b -> Tsil c
zipWith _ Empty _ = Empty
zipWith _ _ Empty = Empty
zipWith f (as :> a) (bs :> b) = Data.Tsil.zipWith f as bs :> f a b

zipWithM :: Monad m => (a -> b -> m c) -> Tsil a -> Tsil b -> m (Tsil c)
zipWithM f as bs = sequenceA (Data.Tsil.zipWith f as bs)

zipWithM_ :: Monad m => (a -> b -> m c) -> Tsil a -> Tsil b -> m ()
zipWithM_ f as bs = sequenceA_ (Data.Tsil.zipWith f as bs)

unzip :: Tsil (a, b) -> (Tsil a, Tsil b)
unzip Empty = (Empty, Empty)
unzip (as :> (a, b)) = (as' :> a, bs' :> b)
  where
    (as', bs') = Data.Tsil.unzip as
