{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Scratch where

import Control.Monad.State
import qualified Data.ByteString.Builder as Builder
import Data.EnumMap (EnumMap)
import Register (FromRegister (fromRegister), RegisterType)
import qualified Register
import System.IO
import Target.X86.Assembly
import Target.X86.Printer.SSA
import Target.X86.RegisterAllocation.Legalisation
import Target.X86.RegisterAllocation3

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

printSSA :: [Instruction Register.Virtual] -> IO ()
printSSA instructions = Builder.hPutBuilder stdout $ printInstructions Register.printVirtual instructions

allocation :: EnumMap Register.Virtual Allocation
allocation = colour (buildGraph splitRegisters) (registerClasses splitRegisters)
