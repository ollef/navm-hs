{-# LANGUAGE OverloadedStrings #-}

module X86.RegisterAllocation where

import Control.Monad
import qualified Data.BitSet as BitSet
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.EnumMap as EnumMap
import Data.EnumSet (EnumSet)
import qualified Data.EnumSet as EnumSet
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Register
import Target.X86.Assembly
import qualified Target.X86.Constraints as Constraints
import qualified Target.X86.Interpreter as Interpreter
import qualified Target.X86.Printer as Printer
import qualified Target.X86.Printer.SSA as Printer.SSA
import qualified Target.X86.Random as Random
import qualified Target.X86.RegisterAllocation as RegisterAllocation
import qualified Target.X86.RegisterAllocation.Legalisation as Legalisation
import qualified Target.X86.RegisterAllocation.SpillInsertion as SpillInsertion

tests :: Hedgehog.Group
tests = Hedgehog.Group "X86.RegisterAllocation" interpretBeforeAndAfterRegisterAllocation

interpretBeforeAndAfterRegisterAllocation :: [(Hedgehog.PropertyName, Hedgehog.Property)]
interpretBeforeAndAfterRegisterAllocation =
  [
    ( "Random instructions"
    , Hedgehog.withTests 1000 $
        Hedgehog.property $ do
          virtualInstructions <- Hedgehog.forAll $ Random.generateSSAInstructions Interpreter.interpretable
          Hedgehog.annotate $ Char8.unpack $ Builder.toLazyByteString $ Printer.SSA.printInstructions Register.printVirtual virtualInstructions
          let virtualInstructions' =
                Register.runVirtualSupply
                  (nextRegister virtualInstructions)
                  $ do
                    Legalisation.concatMapM
                      ( Legalisation.legaliseOperands
                          >=> Legalisation.concatMapM Legalisation.insertMovesAroundConstrainedOccurrences
                      )
                      virtualInstructions
          Hedgehog.annotate $ Char8.unpack $ Builder.toLazyByteString $ Printer.SSA.printInstructions Register.printVirtual virtualInstructions'
          let usedVirtuals = usedRegisters virtualInstructions'
          liveOutsList <- Hedgehog.forAll $ Gen.subsequence $ EnumSet.toList $ usedRegisters virtualInstructions
          let liveOuts = EnumSet.fromList liveOutsList
          (physicalInstructions, allocation) <- allocateRegisters liveOuts virtualInstructions'
          EnumMap.keysSet allocation Hedgehog.=== usedVirtuals
          let virtualState = Interpreter.interprets virtualInstructions'
          let physicalState = Interpreter.interprets physicalInstructions
          compareVirtualPhysicalInterpretation liveOuts allocation virtualState physicalState
    )
  ]

compareVirtualPhysicalInterpretation
  :: Monad m
  => EnumSet Register.Virtual
  -> RegisterAllocation.Allocation
  -> Interpreter.State Register.Virtual
  -> Interpreter.State Register
  -> Hedgehog.PropertyT m ()
compareVirtualPhysicalInterpretation liveOuts allocation virtualState physicalState =
  forM_ (EnumSet.toList liveOuts) $ \virtual -> do
    Hedgehog.annotateShow virtual
    let virtualValue = Interpreter.readRegister virtual virtualState
        physicalValue = case EnumMap.lookup virtual allocation of
          Just (RegisterAllocation.Register physical) -> Interpreter.readRegister physical physicalState
          Just (RegisterAllocation.Stack (RegisterAllocation.StackSlot slot)) ->
            Interpreter.readMemory (Interpreter.readRegister rsp physicalState + 8 * fromIntegral slot) physicalState
          Nothing -> error $ "no allocation for " <> show virtual
    virtualValue Hedgehog.=== physicalValue

allocateRegisters
  :: Monad m
  => EnumSet Register.Virtual
  -> [Instruction Register.Virtual]
  -> Hedgehog.PropertyT m ([Instruction Register], RegisterAllocation.Allocation)
allocateRegisters liveOuts instructions = do
  Hedgehog.annotate $ show graph
  Hedgehog.annotate $ show $ EnumMap.filter (/= BitSet.full) classes
  Hedgehog.annotate $ show allocation
  Hedgehog.annotate $ Char8.unpack $ Builder.toLazyByteString $ Printer.SSA.printInstructions RegisterAllocation.printLocation allocated
  Hedgehog.assert $ all (and . Constraints.mapWithClass (\_ class_ reg -> BitSet.member reg class_)) spilled
  Hedgehog.annotate $ Char8.unpack $ Builder.toLazyByteString $ Printer.printInstructions spilled
  pure (spilled, allocation)
  where
    graph = RegisterAllocation.buildGraph liveOuts instructions
    -- Disable allocating to rsp to avoid conflicting with spilled registers
    classes = EnumMap.map (BitSet.delete rsp) $ RegisterAllocation.registerClasses instructions
    allocation = RegisterAllocation.colour graph classes
    allocated = fmap (fmap (allocation RegisterAllocation.!)) instructions
    spilled = concatMap SpillInsertion.insertSpills allocated

-- scratchedAllocation = RegisterAllocation.useScratchRegisterWhenSafe graph classes allocation
-- coalescedAllocation = RegisterAllocation.coalesce graph classes scratchedAllocation instructions
-- allocated = fmap (fmap (coalescedAllocation RegisterAllocation.!)) instructions
-- irredundant = RegisterAllocation.removeRedundantMoves allocated
-- spilled = concatMap SpillInsertion.insertSpills irredundant

usedRegisters :: Enum register => [Instruction register] -> EnumSet register
usedRegisters = foldMap $ foldMap EnumSet.singleton

nextRegister :: [Instruction Register.Virtual] -> Register.Virtual
nextRegister instructions
  | EnumSet.null used = Register.V 0
  | otherwise = let (Register.V n) = EnumSet.findMax used in Register.V $ n + 1
  where
    used = usedRegisters instructions
