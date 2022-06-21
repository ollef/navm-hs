{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Target.X86.RegisterAllocation where

import Control.Monad.State
import Data.BitSet (BitSet)
import qualified Data.BitSet as BitSet
import Data.Foldable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Hashable
import Data.List (insertBy, sortOn)
import Data.Ord
import qualified Register
import qualified Target.X86.Assembly as X86
import qualified Target.X86.Register.Class as Register.Class

newtype Occurrence = Occurrence
  { id :: Int
  }
  deriving (Eq, Show)

data Virtual
  = Physical !X86.Register
  | Virtual !Register.Virtual
  deriving (Eq, Show)

data CopyOrInstruction register
  = Copy !register !register
  | Instruction !(X86.Instruction register)
  deriving (Eq, Show)

data LiveRange = LiveRange
  { start :: !Int
  , end :: !Int
  }
  deriving (Eq, Show)

instance Semigroup LiveRange where
  LiveRange s1 e1 <> LiveRange s2 e2 = LiveRange (min s1 s2) (max e1 e2)

liveRanges ::
  Hashable register =>
  Int ->
  [X86.Instruction register] ->
  HashMap register LiveRange
liveRanges !time instructions =
  case instructions of
    [] -> mempty
    instruction : instructions' -> do
      let registers = toList instruction
          instructionRanges = foldl' (\l reg -> HashMap.insert reg (LiveRange time time) l) mempty registers
      HashMap.unionWith (<>) instructionRanges $ liveRanges (time + 1) instructions'

newtype StackSlot = StackSlot Int
  deriving (Show, Enum, Bounded, Eq)

data RegisterRange = RegisterRange
  { register :: !Register.Virtual
  , range :: !LiveRange
  }
  deriving (Show)

data Allocation = Register !X86.Register | Stack !StackSlot
  deriving (Show)

data AllocationState = AllocationState
  { active :: [RegisterRange]
  -- ^ Sorted by _decreasing_ end point
  , free :: !(BitSet X86.Register)
  , usedSlots :: !(BitSet StackSlot)
  , allocation :: HashMap Register.Virtual Allocation
  }
  deriving (Show)

type Allocator = State AllocationState

initialState :: AllocationState
initialState =
  AllocationState
    { active = mempty
    , free = BitSet.delete X86.rsp $ Register.Class.toBitSet Register.Class.any
    , usedSlots = mempty
    , allocation = mempty
    }

allocateRegisters :: [X86.Instruction Register.Virtual] -> Allocator ()
allocateRegisters instructions = do
  let inactive =
        sortOn (.range.start) $
          HashMap.foldrWithKey (\register range -> (RegisterRange {..} :)) mempty $
            liveRanges 0 instructions
  forM_ inactive $ \registerRange -> do
    expireOldIntervals registerRange.range.start
    free <- gets (.free)
    case free of
      BitSet.Empty -> spillAt registerRange
      physicalRegister BitSet.:< free' ->
        modify $ \s ->
          s
            { free = free'
            , active = insertBy (flip $ comparing (.range.end)) registerRange s.active
            , allocation = HashMap.insert registerRange.register (Register physicalRegister) s.allocation
            }

expireOldIntervals :: Int -> Allocator ()
expireOldIntervals time = do
  active <- gets (.active)
  let (expiredRanges, active') = span (\registerRange -> registerRange.range.end < time) active
  modify $ \s -> s {active = active'}
  allocation <- gets (.allocation)
  forM_ expiredRanges $ \expiredRange -> do
    case allocation HashMap.! expiredRange.register of
      Register physicalRegister -> modify $ \s -> s {free = BitSet.insert physicalRegister s.free}
      Stack slot -> modify $ \s -> s {usedSlots = BitSet.delete slot s.usedSlots}

spillAt :: RegisterRange -> Allocator ()
spillAt registerRange = do
  slot <- newStackLocation
  active <- gets (.active)
  allocation <- gets (.allocation)
  case active of
    activeRegisterRange : active'
      | activeRegisterRange.range.end >= registerRange.range.end
      , activeRegisterAllocation@(Register _) <- allocation HashMap.! activeRegisterRange.register ->
          modify $ \s ->
            s
              { allocation =
                  HashMap.insert activeRegisterRange.register (Stack slot) $
                    HashMap.insert registerRange.register activeRegisterAllocation allocation
              , active = insertBy (flip $ comparing (.range.end)) registerRange active'
              }
    _ -> modify $ \s -> s {allocation = HashMap.insert registerRange.register (Stack slot) allocation}

newStackLocation :: Allocator StackSlot
newStackLocation = do
  usedSlots <- gets (.usedSlots)
  case BitSet.complementList usedSlots of
    slot : _ -> do
      modify $ \s -> s {usedSlots = BitSet.insert slot s.usedSlots}
      pure slot
    [] -> error "newStackLocation"
