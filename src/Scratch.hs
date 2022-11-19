{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Scratch where

import ArrayBuilder (ArrayBuilder)
import qualified ArrayBuilder
import qualified Data.ByteString.Builder as Builder
import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EnumMap
import qualified Data.Primitive.ByteArray.IO as ByteArray
import Register (FromRegister (fromRegister), RegisterType)
import qualified Register
import System.IO
import qualified Target.ELF as ELF
import Target.X86 as X86
import Target.X86.MachineCode as MachineCode
import Target.X86.Printer.SSA as SSA
import Target.X86.RegisterAllocation as Allocation
import Target.X86.RegisterAllocation.Legalisation
import Target.X86.RegisterAllocation.SpillInsertion

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
    concatMapM insertMovesAroundConstrainedOccurrences legalOperands

graph :: Graph
graph = buildGraph splitRegisters

classes :: EnumMap Register.Virtual X86.Class
classes = registerClasses splitRegisters

allocation :: Allocation
allocation = colour graph classes

scratchedAllocation :: Allocation
scratchedAllocation = useScratchRegisterWhenSafe graph classes allocation

coalescedAllocation :: Allocation
coalescedAllocation = coalesce graph classes scratchedAllocation splitRegisters

allocated :: [Instruction Location]
allocated = map (fmap (allocation EnumMap.!)) splitRegisters

irredundant :: [Instruction Location]
irredundant = removeRedundantMoves allocated

coalesced :: [Instruction Location]
coalesced = map (fmap (coalescedAllocation EnumMap.!)) splitRegisters

coalescedIrredundant :: [Instruction Location]
coalescedIrredundant = removeRedundantMoves coalesced

spilled :: [Instruction X86.Register]
spilled = concatMap insertSpills coalescedIrredundant

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

printSpilled :: [Instruction X86.Register] -> IO ()
printSpilled =
  Builder.hPutBuilder stdout . X86.printInstructions

exit :: MachineCode
exit =
  mconcat
    [ mov rax 1
    , mov rbx 22
    , int 0x80
    ]

assembledExit :: ArrayBuilder
assembledExit = MachineCode.run exit

elfExit :: ArrayBuilder
elfExit = ELF.file assembledExit

writeElfExit :: IO ()
writeElfExit = ByteArray.writeFile "elf-exit" $ ArrayBuilder.run elfExit
