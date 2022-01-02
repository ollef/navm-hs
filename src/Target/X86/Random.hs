module Target.X86.Random where

import Control.Applicative
import Data.Int
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Target.X86.Assembly

generateInstruction :: Gen Instruction
generateInstruction =
  Add <$> generateDestinationOperand <*> generateOperand
    <|> Call <$> generateOperand
    <|> pure Ret
    <|> Mov <$> generateDestinationOperand <*> generateOperand

generateOperand :: Gen Operand
generateOperand =
  Immediate <$> generateImmediate
    <|> generateDestinationOperand

generateMovOperand :: Gen Operand
generateMovOperand =
  Immediate <$> generateMovImmediate
    <|> generateDestinationOperand

generateDestinationOperand :: Gen Operand
generateDestinationOperand =
  Register <$> generateRegister
    <|> Address <$> generateAddress

generateImmediate :: Gen Int64
generateImmediate = fromIntegral <$> Gen.int32 Range.linearBounded

generateMovImmediate :: Gen Int64
generateMovImmediate = Gen.int64 Range.linearBounded

generateRegister :: Gen Register
generateRegister = Gen.enumBounded

generateAddress :: Gen Address
generateAddress =
  Address'
    <$> optional generateRegister
    <*> optional generateScaledRegister
    <*> generateDisplacement
  where
    generateScaledRegister = (,) <$> generateRegister <*> Gen.enumBounded
    generateDisplacement = Gen.int32 Range.linearBounded
