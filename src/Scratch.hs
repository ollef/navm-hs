{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Scratch where

import Control.Monad.State
import qualified Data.ByteString.Builder as Builder
import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EnumMap
import Register (FromRegister (fromRegister), RegisterType)
import qualified Register
import System.IO
import Target.X86 as X86
import Target.X86.Printer.SSA as SSA
import Target.X86.Register.Class as X86.Register
import Target.X86.RegisterAllocation as Allocation
import Target.X86.RegisterAllocation.Legalisation

program :: [Instruction Register.Virtual]
program =
  [ mov a 610
  , mov b 611
  , mul (c, d) a b
  , add e c d
  ]
  where
    a, b, c, d, e :: (RegisterType a ~ Register.Virtual, FromRegister a) => a
    a : b : c : d : e : _ = fromRegister . Register.V <$> [0 ..]

legalOperands :: [Instruction Register.Virtual]
legalOperands = Register.runVirtualSupply (Register.V 100) (concatMapM legaliseOperands program)

splitRegisters :: [Instruction Register.Virtual]
splitRegisters =
  Register.runVirtualSupply (Register.V 200) $
    flip evalStateT mempty $
      concatMapM splitRegistersWithDifferingOccurrenceClasses legalOperands

graph :: Graph
graph = buildGraph splitRegisters

classes :: EnumMap Register.Virtual X86.Register.Class
classes = registerClasses splitRegisters

allocation :: Allocation
allocation = colour graph classes

coalescedAllocation :: Allocation
coalescedAllocation = coalesce graph classes (colour graph classes) splitRegisters

allocated :: [Instruction Location]
allocated = map (fmap (allocation EnumMap.!)) splitRegisters

irredundant :: [Instruction Location]
irredundant = removeRedundantMoves allocated

coalesced :: [Instruction Location]
coalesced = map (fmap (coalescedAllocation EnumMap.!)) splitRegisters

coalescedIrredundant :: [Instruction Location]
coalescedIrredundant = removeRedundantMoves coalesced

printSSA :: [Instruction Register.Virtual] -> IO ()
printSSA instructions = Builder.hPutBuilder stdout $ SSA.printInstructions Register.printVirtual instructions

printAllocated :: [Instruction Location] -> IO ()
printAllocated instructions =
  Builder.hPutBuilder stdout $
    SSA.printInstructions
      ( \alloc -> case alloc of
          Allocation.Register r -> X86.printRegister r
          Allocation.Stack (StackSlot s) -> "[stack slot " <> Builder.wordDec s <> "]"
      )
      instructions
