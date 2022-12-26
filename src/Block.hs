{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Block where

import Openness

data Block node (i :: OC) (o :: OC) where
  OO :: [node 'O 'O] -> Block node 'O 'O
  OC :: [node 'O 'O] -> node 'O 'C -> Block node 'O 'C
  CO :: node 'C 'O -> [node 'O 'O] -> Block node 'C 'O
  CC :: node 'C 'O -> [node 'O 'O] -> node 'O 'C -> Block node 'C 'C

deriving instance (forall i' o'. Show (node i' o')) => Show (Block node i o)

blockView
  :: Block node i o
  -> (MaybeC i (node 'C 'O), [node 'O 'O], MaybeC o (node 'O 'C))
blockView (OO ns) = (NothingC, ns, NothingC)
blockView (OC ns n) = (NothingC, ns, JustC n)
blockView (CO n ns) = (JustC n, ns, NothingC)
blockView (CC n ns n') = (JustC n, ns, JustC n')

pattern Block
  :: MaybeC i (node 'C 'O)
  -> [node 'O 'O]
  -> MaybeC o (node 'O 'C)
  -> Block node i o
pattern Block n ns n' <-
  (blockView -> (n, ns, n'))
  where
    Block NothingC ns NothingC = OO ns
    Block NothingC ns (JustC n) = OC ns n
    Block (JustC n) ns NothingC = CO n ns
    Block (JustC n) ns (JustC n') = CC n ns n'

{-# COMPLETE Block #-}

instance MonoidOC (Block node) where
  type MonoidConstraints (Block node) oc = oc ~ 'O
  empty = Block NothingC mempty NothingC
  append (Block i nodes ~NothingC) (Block ~NothingC nodes' o') =
    Block i (nodes Prelude.<> nodes') o'

instance FunctorOC Block where
  map f (Block n ns n') =
    Block
      ( case n of
          NothingC -> NothingC
          JustC a -> JustC $ f a
      )
      (f <$> ns)
      ( case n' of
          NothingC -> NothingC
          JustC a -> JustC $ f a
      )

instance FoldableOC Block where
  type FoldableConstraints Block a = MonoidConstraints a 'O
  foldMap f (Block n ns n') =
    ( case n of
        NothingC -> empty
        JustC a -> f a
    )
      `append` foldr (append . f) empty ns
      `append` ( case n' of
                  NothingC -> empty
                  JustC a -> f a
               )

instance UnitOC Block where
  unit
    :: forall node i o
     . (Known i, Known o)
    => node i o
    -> Block node i o
  unit node =
    case (known @i, known @o) of
      (SingletonO, SingletonO) -> OO [node]
      (SingletonC, SingletonO) -> CO node []
      (SingletonO, SingletonC) -> OC [] node
      (SingletonC, SingletonC) -> error "impossible"

instance Labelled node => Labelled (Block node) where
  type Label (Block node) = Label node
  label (Block (JustC n) _ _) = label n

instance Successors node => Successors (Block node) where
  successors (Block _ _ (JustC n)) = successors n
