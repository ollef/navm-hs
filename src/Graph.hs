{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Graph where

import Control.Monad.State
import Data.Bifunctor
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.HashSet.Extra as HashSet
import Data.Hashable
import Data.Kind
import GHC.Generics
import Openness

data Graph node (i :: OC) (o :: OC) where
  Single :: node 'O 'O -> Graph node 'O 'O
  CC :: HashMap (Label node) (node 'C 'C) -> Graph node 'C 'C
  OC :: node 'O 'C -> HashMap (Label node) (node 'C 'C) -> Graph node 'O 'C
  CO :: HashMap (Label node) (node 'C 'C) -> node 'C 'O -> Graph node 'C 'O
  OO :: node 'O 'C -> HashMap (Label node) (node 'C 'C) -> node 'C 'O -> Graph node 'O 'O

deriving instance
  (forall i' o'. Show (node i' o'), Show (Label node))
  => Show (Graph node i o)

manyView
  :: Graph node i o
  -> Maybe
      ( MaybeO i (node 'O 'C)
      , HashMap (Label node) (node 'C 'C)
      , MaybeO o (node 'C 'O)
      )
manyView (Single _) = Nothing
manyView (CC ls) = Just (NothingO, ls, NothingO)
manyView (OC n ls) = Just (JustO n, ls, NothingO)
manyView (CO ls n) = Just (NothingO, ls, JustO n)
manyView (OO n ls n') = Just (JustO n, ls, JustO n')

pattern Many
  :: MaybeO i (node 'O 'C)
  -> HashMap (Label node) (node 'C 'C)
  -> MaybeO o (node 'C 'O)
  -> Graph node i o
pattern Many n ls n' <-
  (manyView -> Just (n, ls, n'))
  where
    Many NothingO ls NothingO = CC ls
    Many (JustO n) ls NothingO = OC n ls
    Many NothingO ls (JustO n) = CO ls n
    Many (JustO n) ls (JustO n') = OO n ls n'

{-# COMPLETE Single, Many #-}

{-# COMPLETE Single, CC, OC, CO, OO #-}

instance (MonoidOC node, HashLabelled node) => MonoidOC (Graph node) where
  type MonoidConstraints (Graph node) oc = MonoidConstraints node 'O
  empty :: forall oc. (Known oc, MonoidConstraints node 'O) => Graph node oc oc
  empty = case known @oc of
    SingletonC -> CC mempty
    SingletonO -> Single empty
  append (Single a) (Single b) = Single (append a b)
  append (Single a) (Many (JustO i) ls o) = Many (JustO (append a i)) ls o
  append (Many i ls (JustO o)) (Single a) = Many i ls (JustO (append o a))
  append (Many i labels NothingO) (Many NothingO labels' o) =
    Many i (labels <> labels') o
  append (Many i labels (JustO o)) (Many (JustO i') labels' o') =
    Many i (HashMap.insert (label o) (append o i') $ labels <> labels') o'

instance FunctorOC Graph where
  type FunctorConstraints Graph a = HashLabelled a
  map f (Single n) = Single $ f n
  map f (Many i ls o) =
    Many
      ( case i of
          NothingO -> NothingO
          JustO a -> JustO $ f a
      )
      (HashMap.fromList [(label n', n') | n <- HashMap.elems ls, let n' = f n])
      ( case o of
          NothingO -> NothingO
          JustO a -> JustO $ f a
      )

instance FoldableOC Graph where
  type FoldableConstraints Graph b = MonoidConstraints b 'C
  foldMap f (Single n) = f n
  foldMap f (Many i ls o) =
    ( case i of
        NothingO -> empty
        JustO a -> f a
    )
      `append` foldr (append . f) empty (HashMap.elems ls)
      `append` ( case o of
                  NothingO -> empty
                  JustO a -> f a
               )

instance UnitOC Graph where
  type UnitConstraints Graph a = HashLabelled a
  unit
    :: forall a i o
     . (Known i, Known o, UnitConstraints Graph a)
    => a i o
    -> Graph a i o
  unit node = case (known @i, known @o) of
    (SingletonC, SingletonC) -> CC $ HashMap.singleton (label node) node
    (SingletonO, SingletonC) -> OC node mempty
    (SingletonC, SingletonO) -> CO mempty node
    (SingletonO, SingletonO) -> Single node

data SomeNode (node :: OC -> OC -> Type) where
  Entry :: node 'O 'C -> SomeNode node
  Internal :: node 'C 'C -> SomeNode node
  Exit :: node 'C 'O -> SomeNode node
  EntryExit :: node 'O 'O -> SomeNode node

withSome :: SomeNode node -> (forall i o. node i o -> a) -> a
withSome sn f = case sn of
  Entry n -> f n
  Internal n -> f n
  Exit n -> f n
  EntryExit n -> f n

data SomeLabel node
  = EntryLabel
  | NodeLabel !(Label node)
  deriving (Generic)

deriving instance Eq (Label node) => Eq (SomeLabel node)

deriving instance Ord (Label node) => Ord (SomeLabel node)

deriving instance Show (Label node) => Show (SomeLabel node)

deriving instance Hashable (Label node) => Hashable (SomeLabel node)

someLabel :: Labelled node => SomeNode node -> SomeLabel node
someLabel Entry {} = EntryLabel
someLabel (Internal n) = NodeLabel $ label n
someLabel (Exit n) = NodeLabel $ label n
someLabel EntryExit {} = EntryLabel

someSuccessors
  :: (Successors node, Hashable (Label node))
  => SomeNode node
  -> HashSet (Label node)
someSuccessors = \case
  Entry entry -> successors entry
  Internal n -> successors n
  Exit _ -> mempty
  EntryExit _ -> mempty

(!)
  :: (Labelled node, Hashable (Label node))
  => Graph node i o
  -> SomeLabel node
  -> SomeNode node
Single node ! EntryLabel = EntryExit node
Single _ ! NodeLabel _ = error "Graph.!: Single/NodeLabel"
Many (JustO entry) _internal _exit ! EntryLabel = Entry entry
Many NothingO _internal _exit ! EntryLabel = error "Graph.!: Many/EntryLabel: no entry"
Many _entry internal NothingO ! NodeLabel l
  | Just n <- HashMap.lookup l internal = Internal n
  | otherwise = error "Graph.!: no such label"
Many _entry internal (JustO exit) ! NodeLabel l
  | l == label exit = Exit exit
  | Just n <- HashMap.lookup l internal = Internal n
  | otherwise = error "Graph.!: no such label"

reversePostOrder :: (Successors node, Hashable (Label node)) => Graph node 'O x -> [SomeNode node]
reversePostOrder (Single node) = [EntryExit node]
reversePostOrder graph@(Many (JustO entry) _nodes _exit) =
  fst $
    flip execState mempty $
      forM_ (HashSet.toList $ successors entry) go
  where
    go l = do
      visited <- gets snd
      let (visited', already) = HashSet.insertMember l visited
      unless already do
        let node = graph ! NodeLabel l
        modify $ second $ const visited'
        forM_ (HashSet.toList $ someSuccessors node) go
        modify $ first (node :)

postOrder :: (Successors node, Hashable (Label node)) => Graph node 'O x -> [SomeNode node]
postOrder = reverse . reversePostOrder

predecessors
  :: (Hashable (Label node), Successors node)
  => Graph node 'O x
  -> HashMap (Label node) (HashSet (SomeLabel node))
predecessors (Single _) = mempty
predecessors (Many (JustO entry) nodes _exit) =
  HashMap.unionWith
    (<>)
    ( HashMap.fromListWith
        (<>)
        [ (s, HashSet.singleton (NodeLabel l))
        | (l, node) <- HashMap.toList nodes
        , s <- HashSet.toList $ successors node
        ]
    )
    ((\ ~() -> HashSet.singleton EntryLabel) <$> HashSet.toMap (successors entry))
