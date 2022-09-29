{-# LANGUAGE DuplicateRecordFields #-}
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
      Just (_, _, reg, queue') ->
        reg : do
          let neighbours = EnumMap.findWithDefault mempty reg graph
          go $ EnumSet.foldl' (\q n -> snd $ PSQ.alter ((,) () . fmap (first (+ 1))) (coerce n) q) queue' neighbours

newtype StackSlot = StackSlot Word
  deriving (Show, Enum, Bounded, Eq, Bits, FiniteBits, Num)

data Allocation = Register !X86.Register | Stack !StackSlot
  deriving (Eq, Show)

scratchRegister :: (RegisterType a ~ X86.Register, FromRegister a) => a
scratchRegister = X86.r15

colour :: Graph -> EnumMap Register.Virtual X86.Register.Class -> EnumMap Register.Virtual Allocation
colour graph classes = foldl' go mempty orderedRegisters
  where
    orderedRegisters :: [(Register.Virtual, X86.Register.Class)]
    orderedRegisters = sortOn ((/= 1) . BitSet.size . snd) [(reg, classes EnumMap.! reg) | reg <- simplicialEliminationOrder graph]
    go
      :: EnumMap Register.Virtual Allocation
      -> (Register.Virtual, X86.Register.Class)
      -> EnumMap Register.Virtual Allocation
    go allocations (reg, class_) = do
      let neighbours = EnumMap.findWithDefault mempty reg graph
          neighbourRegisters =
            mconcat
              [ Register.fromRegister physicalReg
              | neighbour <- EnumSet.toList neighbours
              , Just (Register physicalReg) <- [EnumMap.lookup neighbour allocations]
              ]
          possibleRegisters = BitSet.delete scratchRegister $ BitSet.intersection class_ (BitSet.complement neighbourRegisters)
      case possibleRegisters of
        physicalReg BitSet.:< _ ->
          EnumMap.insert reg (Register physicalReg) allocations
        BitSet.Empty -> do
          let neighbourSlots =
                EnumSet.fromList
                  [ s
                  | neighbour <- EnumSet.toList neighbours
                  , Just (Stack s) <- [EnumMap.lookup neighbour allocations]
                  ]
              slot = case EnumSet.maxView neighbourSlots of
                Nothing -> 0
                Just (s, _) -> s + 1
          EnumMap.insert reg (Stack slot) allocations

removeRedundantMoves :: Eq r => [X86.Instruction r] -> [X86.Instruction r]
removeRedundantMoves = concatMap go
  where
    go (X86.Mov (X86.Register a) (X86.Register b)) | a == b = []
    go instr = [instr]

coalesce
  :: Graph
  -> EnumMap Register.Virtual X86.Register.Class
  -> [X86.Instruction Register.Virtual]
  -> EnumMap Register.Virtual Allocation
  -> EnumMap Register.Virtual Allocation
coalesce graph classes instructions initialAllocation =
  foldl' go initialAllocation [(r1, r2) | X86.Mov (X86.Register r1) (X86.Register r2) <- instructions, r1 /= r2]
  where
    go
      :: EnumMap Register.Virtual Allocation
      -> (Register.Virtual, Register.Virtual)
      -> EnumMap Register.Virtual Allocation
    go allocation (r1, r2)
      | colour1 == colour2 = allocation
      | EnumSet.member r1 neighbours = allocation
      | otherwise = do
          let class_ = BitSet.intersection (classes EnumMap.! r1) (classes EnumMap.! r2)
              neighbourRegisters =
                EnumSet.foldl'
                  ( \used neighbour ->
                      used <> case allocation EnumMap.! neighbour of
                        Register r -> BitSet.singleton r
                        Stack _ -> mempty
                  )
                  mempty
                  neighbours
              possibleRegisters = BitSet.delete scratchRegister $ BitSet.intersection class_ (BitSet.complement neighbourRegisters)
          case possibleRegisters of
            physicalReg BitSet.:< _ -> EnumMap.insert r1 (Register physicalReg) $ EnumMap.insert r2 (Register physicalReg) allocation
            BitSet.Empty -> allocation
      where
        neighbours = graph EnumMap.! r1 <> graph EnumMap.! r2
        colour1 = allocation EnumMap.! r1
        colour2 = allocation EnumMap.! r2
