{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Scratch where

import Control.Monad.State
import Data.ByteString.Builder (hPutBuilder)
import Data.HashMap.Lazy (HashMap)
import Register
import System.IO (stdout)
import Target.X86
import qualified Target.X86.Printer.SSA as SSA
import Target.X86.RegisterAllocation as RegisterAllocation
import Target.X86.RegisterAllocation.Legalisation

program :: [Instruction Register.Virtual]
program =
  [ add a b c
  , add d a a
  , add e a a
  , add f a a
  , add g a a
  , add i a a
  , add h a a
  , mul (j, k) c d
  , mul (l, m) c d
  ]
  where
    a, b, c, d, e, f, g, h, i, j, k, l, m :: (FromRegister a, RegisterType a ~ Register.Virtual) => a
    a : b : c : d : e : f : g : h : i : j : k : l : m : _ = fromRegister . Register.V <$> [0 ..]

legalisedProgram :: [Instruction Register.Virtual]
legalisedProgram =
  runVirtualSupply (V 20) $
    concatMapM legaliseOperands program

splitProgram :: [Instruction Register.Virtual]
splitProgram =
  runVirtualSupply (V 200) $
    flip evalStateT mempty $
      concatMapM splitRegistersWithDifferingOccurrenceClasses legalisedProgram

allocation :: HashMap Register.Virtual Allocation
allocation = (runVirtualSupply (V 2000) $ execStateT allocateRegisters $ initialState splitProgram).allocation

copies :: [Copy]
copies = (runVirtualSupply (V 2000) $ execStateT allocateRegisters $ initialState splitProgram).copies

allocatedProgram :: [Instruction Allocation]
allocatedProgram = runVirtualSupply (V 2000) $ RegisterAllocation.run splitProgram

printV :: [Instruction Register.Virtual] -> IO ()
printV =
  hPutBuilder stdout
    . SSA.printInstructions Register.printVirtual

printP :: [Instruction Register] -> IO ()
printP =
  hPutBuilder stdout . printInstructions

printA :: [Instruction Allocation] -> IO ()
printA =
  hPutBuilder stdout
    . SSA.printInstructions printAllocation
