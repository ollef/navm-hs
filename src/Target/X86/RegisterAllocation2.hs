{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Target.X86.RegisterAllocation2 where

import Control.Monad.State
import Data.BitSet (BitSet)
import qualified Data.BitSet as BitSet
import Data.Bits
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import Data.Foldable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Hashable
import qualified Data.IntervalMap.Generic.Interval as IntervalMap
import Data.IntervalMap.Generic.Lazy (IntervalMap)
import qualified Data.IntervalMap.Generic.Lazy as IntervalMap
import Data.List (insertBy, sortOn)
import Data.Ord
import qualified Register
import qualified Target.X86.Assembly as X86
import qualified Target.X86.Printer as X86
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

data RegisterInterval = RegisterInterval
  { register :: !Register.Virtual
  , start :: !Int
  , end :: !Int
  }
  deriving (Eq, Ord, Show)

instance IntervalMap.Interval RegisterInterval Int where
  lowerBound = (.start)
  upperBound = (.end)

type Intervals = IntervalMap RegisterInterval Register.Class

liveIntervals :: [X86.Instruction Register.Virtual] -> Intervals
liveIntervals instructions =
  HashMap.foldMapWithKey
    ( \register range ->
        IntervalMap.singleton
          ( RegisterInterval
              { register
              , start = range.start
              , end = range.end
              }
          )
          range.class_
    )
    $ liveRanges 0 instructions
