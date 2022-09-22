{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Target.X86.RegisterAllocation3 where

import Data.Bifunctor
import qualified Data.BitSet as BitSet
import Data.Coerce
import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EnumMap
import Data.EnumSet (EnumSet)
import qualified Data.EnumSet as EnumSet
import Data.Foldable
import Data.IntPSQ (IntPSQ)
import qualified Data.IntPSQ as PSQ
import Data.Ord
import qualified Register
import qualified Target.X86.Assembly as X86
import qualified Target.X86.Register.Class as X86.Register

type Graph = EnumMap Register.Virtual (EnumSet Register.Virtual)

addEdge :: Register.Virtual -> Register.Virtual -> Graph -> Graph
addEdge r1 r2 =
  EnumMap.insertWith (<>) r1 (EnumSet.singleton r2)
    . EnumMap.insertWith (<>) r2 (EnumSet.singleton r1)

addEdges :: [(Register.Virtual, Register.Virtual)] -> Graph -> Graph
addEdges edges graph = foldl' (flip $ uncurry addEdge) graph edges

buildGraph :: [X86.Instruction Register.Virtual] -> Graph
buildGraph =
  fst . foldr go mempty
  where
    go
      :: X86.Instruction Register.Virtual
      -> (Graph, EnumSet Register.Virtual)
      -> (Graph, EnumSet Register.Virtual)
    go instruction (graph, liveOuts) = (graph', liveIns)
      where
        liveIns =
          X86.Register.foldWithClass
            ( \occ _ reg -> case occ of
                X86.Register.Definition -> EnumSet.delete reg
                X86.Register.Use -> EnumSet.insert reg
            )
            liveOuts
            instruction

        graph' = case instruction of
          X86.Mov (X86.Register dst) (X86.Register src) ->
            addEdges [(reg, dst) | reg <- EnumSet.toList liveOuts, reg /= src, reg /= dst] graph
          _ ->
            X86.Register.foldWithClass
              ( \occ _ reg ->
                  case occ of
                    X86.Register.Definition -> addEdges [(reg, reg') | reg' <- EnumSet.toList liveOuts, reg /= reg']
                    X86.Register.Use -> id
              )
              graph
              instruction

registerClasses :: [X86.Instruction Register.Virtual] -> EnumMap Register.Virtual X86.Register.Class
registerClasses =
  EnumMap.unionsWith BitSet.intersection
    . concatMap (toList . X86.Register.mapWithClass (\_ class_ reg -> EnumMap.singleton reg class_))

simplicialEliminationOrder :: Graph -> [Register.Virtual]
simplicialEliminationOrder graph = go $ PSQ.fromList [(coerce r, Down 0, r) | (r, _) <- EnumMap.toList graph]
  where
    go :: IntPSQ (Down Int) Register.Virtual -> [Register.Virtual]
    go queue = case PSQ.minView queue of
      Nothing -> []
      Just (_, _, r, queue') ->
        r : do
          let neighbours = graph EnumMap.! r
          go $ EnumSet.foldl' (\q n -> snd $ PSQ.alter ((,) () . fmap (first (+ 1))) (coerce n) q) queue' neighbours
