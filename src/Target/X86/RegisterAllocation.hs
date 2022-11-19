{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

module Target.X86.RegisterAllocation where

import Data.Bifunctor
import qualified Data.BitSet as BitSet
import Data.Bits
import Data.Coerce
import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EnumMap
import Data.EnumSet (EnumSet)
import qualified Data.EnumSet as EnumSet
import Data.Foldable
import Data.IntPSQ (IntPSQ)
import qualified Data.IntPSQ as PSQ
import Data.List (sortOn)
import Data.Ord
import Register (FromRegister, RegisterType)
import qualified Register
import qualified Target.X86.Assembly as X86
import Target.X86.Constraints
import qualified Target.X86.Register as X86.Register

type Graph = EnumMap Register.Virtual (EnumSet Register.Virtual)

type Classes = EnumMap Register.Virtual X86.Register.Class

type Allocation = EnumMap Register.Virtual Location

newtype StackSlot = StackSlot Word
  deriving (Show, Enum, Bounded, Eq, Bits, FiniteBits, Num)

data Location = Register !X86.Register | Stack !StackSlot
  deriving (Eq, Show)

addEdge :: Register.Virtual -> Register.Virtual -> Graph -> Graph
addEdge r1 r2 =
  EnumMap.insertWith (<>) r1 (EnumSet.singleton r2)
    . EnumMap.insertWith (<>) r2 (EnumSet.singleton r1)

addEdges :: [(Register.Virtual, Register.Virtual)] -> Graph -> Graph
addEdges edges graph = foldl' (flip $ uncurry addEdge) graph edges

delete :: Register.Virtual -> Graph -> Graph
delete r g =
  EnumSet.foldl' (flip $ EnumMap.adjust (EnumSet.delete r)) g' $ fold maybeEdges
  where
    (maybeEdges, g') = EnumMap.updateLookupWithKey (\_ _ -> Nothing) r g

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
          foldWithClass
            ( \occ _ reg -> case occ of
                Definition -> EnumSet.delete reg
                Use -> EnumSet.insert reg
            )
            liveOuts
            instruction

        graph' = case instruction of
          X86.Mov (X86.Register dst) (X86.Register src) ->
            addEdges [(reg, dst) | reg <- EnumSet.toList liveOuts, reg /= src, reg /= dst] graph
          _ ->
            foldWithClass
              ( \occ _ reg ->
                  case occ of
                    Definition -> addEdges [(reg, reg') | reg' <- EnumSet.toList liveOuts, reg /= reg']
                    Use -> id
              )
              graph
              instruction

registerClasses :: [X86.Instruction Register.Virtual] -> Classes
registerClasses =
  EnumMap.unionsWith BitSet.intersection
    . concatMap (toList . mapWithClass (\_ class_ reg -> EnumMap.singleton reg class_))

simplicialEliminationOrder :: Graph -> [Register.Virtual]
simplicialEliminationOrder graph = go $ PSQ.fromList [(coerce r, Down 0, r) | (r, _) <- EnumMap.toList graph]
  where
    go :: IntPSQ (Down Int) Register.Virtual -> [Register.Virtual]
    go queue = case PSQ.minView queue of
      Nothing -> []
      Just (_, _, reg, queue') ->
        reg : do
          let neighbours = EnumMap.findWithDefault mempty reg graph
          go $ EnumSet.foldl' (\q n -> snd $ PSQ.alter ((,) () . fmap (first (+ 1))) (coerce n) q) queue' neighbours

scratchRegister :: (RegisterType a ~ X86.Register, FromRegister a) => a
scratchRegister = X86.r15

colour :: Graph -> Classes -> Allocation
colour graph classes = foldl' go mempty orderedRegisters
  where
    orderedRegisters :: [(Register.Virtual, X86.Register.Class)]
    orderedRegisters = sortOn ((/= 1) . BitSet.size . snd) [(reg, classes EnumMap.! reg) | reg <- simplicialEliminationOrder graph]
    go :: Allocation -> (Register.Virtual, X86.Register.Class) -> Allocation
    go allocation (reg, class_) = do
      let neighbours = EnumMap.findWithDefault mempty reg graph
          neighbourRegisters =
            mconcat
              [ Register.fromRegister physicalReg
              | neighbour <- EnumSet.toList neighbours
              , Just (Register physicalReg) <- [EnumMap.lookup neighbour allocation]
              ]
          possibleRegisters = BitSet.delete scratchRegister $ BitSet.difference class_ neighbourRegisters
      case possibleRegisters of
        physicalReg BitSet.:< _ ->
          EnumMap.insert reg (Register physicalReg) allocation
        BitSet.Empty -> do
          let neighbourSlots =
                BitSet.fromList
                  [ s
                  | neighbour <- EnumSet.toList neighbours
                  , Just (Stack s) <- [EnumMap.lookup neighbour allocation]
                  ]
              slot = case BitSet.complementList neighbourSlots of
                [] -> error "impossible: no slots"
                s : _ -> s
          EnumMap.insert reg (Stack slot) allocation

removeRedundantMoves :: Eq r => [X86.Instruction r] -> [X86.Instruction r]
removeRedundantMoves = concatMap go
  where
    go (X86.Mov (X86.Register a) (X86.Register b)) | a == b = []
    go instr = [instr]

coalesce :: Graph -> Classes -> Allocation -> [X86.Instruction Register.Virtual] -> Allocation
coalesce initialGraph initialClasses initialAllocation instructions = do
  let (_, _, renamedAllocation, renaming, renamedRegisters) =
        Register.runVirtualSupply (Register.V $ lastRegisterNum + 1) $
          foldlM go (initialGraph, initialClasses, initialAllocation, mempty, mempty) [(r1, r2) | X86.Mov (X86.Register r1) (X86.Register r2) <- instructions, r1 /= r2]
  EnumSet.foldl'
    (\allocation original -> EnumMap.insert original (EnumMap.findWithDefault (error $ "no allocation " <> show (original, renamedAllocation, renaming)) (renamed original renaming) renamedAllocation) allocation)
    initialAllocation
    renamedRegisters
  where
    (Register.V lastRegisterNum, _) = EnumMap.findMax initialAllocation
    renamed r renaming
      | r' == r = r'
      | otherwise = renamed r' renaming
      where
        r' = EnumMap.findWithDefault r r renaming
    go
      :: (Graph, Classes, Allocation, EnumMap Register.Virtual Register.Virtual, EnumSet Register.Virtual)
      -> (Register.Virtual, Register.Virtual)
      -> Register.VirtualSupply (Graph, Classes, Allocation, EnumMap Register.Virtual Register.Virtual, EnumSet Register.Virtual)
    go unchanged@(!graph, !classes, !allocation, !renaming, !renamedRegisters) (r1, r2)
      | r1' == r2' = pure unchanged
      | EnumSet.member r1' neighbours = pure unchanged
      | locationType l1 /= locationType l2 = pure unchanged
      | otherwise = do
          let class_ = BitSet.intersection (classes EnumMap.! r1') (classes EnumMap.! r2')
              neighbourRegisters =
                BitSet.fromList
                  [ r
                  | neighbour <- EnumSet.toList neighbours
                  , Register r <- [allocation EnumMap.! neighbour]
                  ]
              possibleRegisters = BitSet.delete scratchRegister $ BitSet.difference class_ neighbourRegisters
              location = case possibleRegisters of
                BitSet.Empty -> do
                  let neighbourSlots =
                        BitSet.fromList
                          [ s
                          | neighbour <- EnumSet.toList neighbours
                          , Stack s <- [allocation EnumMap.! neighbour]
                          ]
                      slot = case BitSet.complementList neighbourSlots of
                        [] -> error "impossible: no stack slots"
                        s : _ -> s
                  Stack slot
                physicalReg BitSet.:< _ -> Register physicalReg
          r <- Register.fresh
          let graph' = addEdges [(r, n) | n <- EnumSet.toList neighbours] $ delete r1' $ delete r2' graph
              classes' = EnumMap.insert r class_ $ EnumMap.delete r1' $ EnumMap.delete r2' classes
              allocation' = EnumMap.insert r location $ EnumMap.delete r1' $ EnumMap.delete r2' allocation
              renaming' = EnumMap.insert r1' r $ EnumMap.insert r2' r renaming
              renamedRegisters' = EnumSet.insert r1 $ EnumSet.insert r2 renamedRegisters
          pure (graph', classes', allocation', renaming', renamedRegisters')
      where
        r1' = renamed r1 renaming
        r2' = renamed r2 renaming
        l1 = allocation EnumMap.! r1'
        l2 = allocation EnumMap.! r2'
        locationType :: Location -> Int
        locationType Register {} = 0
        locationType Stack {} = 1
        neighbours = graph EnumMap.! r1' <> graph EnumMap.! r2'
