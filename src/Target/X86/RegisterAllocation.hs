{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Target.X86.RegisterAllocation where

import Control.Monad.State
import Data.BitSet (BitSet)
import qualified Data.BitSet as BitSet
import Data.Bits
import Data.Foldable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Hashable
import Data.List (insertBy, sortOn)
import Data.Ord
import qualified Register
import qualified Target.X86.Assembly as X86
import qualified Target.X86.Register.Class as Register (Class)
import qualified Target.X86.Register.Class as Register.Class

data LiveRange = LiveRange
  { start :: !Int
  , end :: !Int
  , class_ :: !Register.Class
  }
  deriving (Eq, Show)

instance Semigroup LiveRange where
  LiveRange s1 e1 c1 <> LiveRange s2 e2 c2 =
    LiveRange (min s1 s2) (max e1 e2) (BitSet.intersection c1 c2)

liveRanges ::
  Hashable register =>
  Int ->
  [X86.Instruction register] ->
  HashMap register LiveRange
liveRanges !time instructions =
  case instructions of
    [] -> mempty
    instruction : instructions' -> do
      let registers = toList $ Register.Class.mapWithClass (\_ c r -> (c, r)) instruction
          instructionRanges = foldl' (\l (class_, reg) -> HashMap.insert reg (LiveRange time time class_) l) mempty registers
      HashMap.unionWith (<>) instructionRanges $ liveRanges (time + 1) instructions'

newtype StackSlot = StackSlot Word
  deriving (Show, Enum, Bounded, Eq, Bits, FiniteBits)

data RegisterRange = RegisterRange
  { register :: !Register.Virtual
  , range :: !LiveRange
  }
  deriving (Show)

data Allocation = Register !X86.Register | Stack !StackSlot
  deriving (Show)

data AllocationState = AllocationState
  { inactive :: [RegisterRange]
  -- ^ Sorted by increasing start point
  , active :: [RegisterRange]
  -- ^ Sorted by increasing end point
  , free :: !(BitSet X86.Register)
  , usedSlots :: !(BitSet StackSlot)
  , allocation :: HashMap Register.Virtual Allocation
  }
  deriving (Show)

type Allocator = State AllocationState

initialState :: [X86.Instruction Register.Virtual] -> AllocationState
initialState instructions =
  AllocationState
    { inactive =
        sortOn (.range.start) $
          HashMap.foldrWithKey (\register range -> (RegisterRange {..} :)) mempty $
            liveRanges 0 instructions
    , active = mempty
    , free = BitSet.delete X86.rsp Register.Class.any
    , usedSlots = mempty
    , allocation = mempty
    }

allocateRegisters :: Allocator ()
allocateRegisters = do
  inactive <- gets (.inactive)
  case inactive of
    [] -> pure ()
    registerRange : inactive' -> do
      modify $ \s -> s {inactive = inactive'}
      expireOldIntervals registerRange.range.start
      free <- gets (.free)
      case BitSet.intersection registerRange.range.class_ free of
        BitSet.Empty -> spillAt registerRange
        physicalRegister BitSet.:< _ ->
          modify $ \s ->
            s
              { free = BitSet.delete physicalRegister free
              , active = insertBy (comparing (.range.end)) registerRange s.active
              , allocation = HashMap.insert registerRange.register (Register physicalRegister) s.allocation
              }
      allocateRegisters

expireOldIntervals :: Int -> Allocator ()
expireOldIntervals time = do
  active <- gets (.active)
  let (expiredRanges, active') = span (\registerRange -> registerRange.range.end < time) active
  modify $ \s -> s {active = active'}
  allocation <- gets (.allocation)
  forM_ expiredRanges $ \expiredRange ->
    case allocation HashMap.! expiredRange.register of
      Register physicalRegister -> modify $ \s -> s {free = BitSet.insert physicalRegister s.free}
      Stack slot -> modify $ \s -> s {usedSlots = BitSet.delete slot s.usedSlots}

spillAt :: RegisterRange -> Allocator ()
spillAt registerRange = do
  slot <- newStackLocation
  active <- gets (.active)
  allocation <- gets (.allocation)
  let relevant activeRegisterRange
        | Register r <- allocation HashMap.! activeRegisterRange.register =
            BitSet.member r registerRange.range.class_
        | otherwise = False
      (prefix, suffix) = span (not . relevant) active
  case suffix of
    activeRegisterRange : suffix'
      | activeRegisterRange.range.end >= registerRange.range.end
      , activeRegisterAllocation@(Register _) <- allocation HashMap.! activeRegisterRange.register ->
          modify $ \s ->
            s
              { allocation =
                  HashMap.insert activeRegisterRange.register (Stack slot) $
                    HashMap.insert registerRange.register activeRegisterAllocation allocation
              , active = insertBy (comparing (.range.end)) registerRange $ prefix <> suffix'
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
