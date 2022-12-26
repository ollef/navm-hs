{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Openness where

import Data.HashSet (HashSet)
import Data.Hashable
import Data.Kind

data OC = C | O

data MaybeO oc a where
  NothingO :: MaybeO 'C a
  JustO :: a -> MaybeO 'O a

data MaybeC oc a where
  NothingC :: MaybeC 'O a
  JustC :: a -> MaybeC 'C a

class Known (oc :: OC) where
  known :: Singleton oc

data Singleton (oc :: OC) where
  SingletonO :: Singleton 'O
  SingletonC :: Singleton 'C

instance Known 'O where
  known = SingletonO

instance Known 'C where
  known = SingletonC

class MonoidOC (node :: OC -> OC -> Type) where
  type MonoidConstraints node (oc :: OC) :: Constraint
  type MonoidConstraints node oc = ()
  empty :: (Known oc, MonoidConstraints node oc) => node oc oc
  append :: MonoidConstraints node o => node i o -> node o o' -> node i o'
  infixr 6 `append`

class FunctorOC (node :: (OC -> OC -> Type) -> OC -> OC -> Type) where
  type FunctorConstraints node (b :: OC -> OC -> Type) :: Constraint
  type FunctorConstraints node b = ()
  map :: FunctorConstraints node b => (forall i' o'. a i' o' -> b i' o') -> node a i o -> node b i o

class FunctorOC node => FoldableOC node where
  type FoldableConstraints node (b :: OC -> OC -> Type) :: Constraint
  type FoldableConstraints node b = ()
  foldMap
    :: (FoldableConstraints node b, MonoidOC b)
    => (forall i' o'. a i' o' -> b i' o')
    -> node a i o
    -> b i o

foldMapA
  :: (FoldableOC node, FoldableConstraints node (ComposeOC f b), MonoidOC b, Applicative f)
  => (forall i' o'. a i' o' -> f (b i' o'))
  -> node a i o
  -> f (b i o)
foldMapA f = getComposeOC . Openness.foldMap (ComposeOC . f)

class FunctorOC node => UnitOC (node :: (OC -> OC -> Type) -> OC -> OC -> Type) where
  type UnitConstraints node (a :: OC -> OC -> Type) :: Constraint
  type UnitConstraints node a = ()
  unit :: (Known i, Known o, UnitConstraints node a) => a i o -> node a i o

class Labelled (node :: OC -> OC -> Type) where
  type Label node
  label :: node 'C o -> Label node

class (Eq (Label node), Hashable (Label node), Labelled node) => HashLabelled node

instance (Eq (Label node), Hashable (Label node), Labelled node) => HashLabelled node

class Labelled node => Successors node where
  successors :: node i 'C -> HashSet (Label node)

newtype ConstOC a (i :: OC) (o :: OC) = ConstOC {getConstOC :: a}

instance Monoid a => MonoidOC (ConstOC a) where
  empty = ConstOC mempty
  append (ConstOC a) (ConstOC b) = ConstOC $ a <> b

newtype ComposeOC f g (i :: OC) (o :: OC) = ComposeOC {getComposeOC :: f (g i o)}

instance (MonoidOC g, Applicative f) => MonoidOC (ComposeOC f g) where
  type MonoidConstraints (ComposeOC f g) oc = MonoidConstraints g oc
  empty = ComposeOC $ pure empty
  append (ComposeOC a) (ComposeOC b) = ComposeOC $ append <$> a <*> b

instance Functor f => FunctorOC (ComposeOC f) where
  map f (ComposeOC a) = ComposeOC $ f <$> a

instance Applicative f => UnitOC (ComposeOC f) where
  unit a = ComposeOC $ pure a
