{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Graph.Analysis where

import Control.Monad.State hiding (join)
import Data.Bifunctor
import Data.Foldable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.HashSetList as HashSetList
import Data.Hashable
import Graph (Graph)
import qualified Graph
import Openness

class DataflowLattice a where
  bottom :: a
  default bottom :: Monoid a => a
  bottom = mempty
  join :: a -> a -> a
  default join :: Semigroup a => a -> a -> a
  join = (<>)

data Changed a = Changed {changed :: !Bool, value :: !a}
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

forward
  :: (DataflowLattice a, Successors node, Hashable (Label node))
  => (forall i o. a -> node i o -> Changed a)
  -> Graph node 'O x
  -> HashMap (Graph.SomeLabel node) a
forward transfer graph =
  fst $
    flip execState (mempty, HashSetList.fromList $ Graph.someLabel <$> Graph.reversePostOrder graph) do
      todo <- gets snd
      case todo of
        HashSetList.Empty -> pure ()
        nodeLabel HashSetList.:< todo' -> do
          modify $ second $ const todo'
          let preds = case nodeLabel of
                Graph.EntryLabel -> mempty
                Graph.NodeLabel l -> HashMap.findWithDefault mempty l predecessors
          outs <- gets fst
          let predOuts = [HashMap.findWithDefault bottom p outs | p <- HashSet.toList preds]
              inState = foldl' join bottom predOuts
              node = graph Graph.! nodeLabel
              out = Graph.withSome node $ transfer inState
          when out.changed do
            modify $ bimap (HashMap.insert nodeLabel out.value) (HashSetList.fromSet (HashSet.map Graph.NodeLabel $ Graph.someSuccessors node) <>)
  where
    predecessors = Graph.predecessors graph
