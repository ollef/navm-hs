module Target.X86.Random where

import Control.Applicative
import Data.Int
import Target.X86.Assembly

generateInstructions :: [Instruction]
generateInstructions =
  Add <$> generateOperand <*> generateOperand
    <|> Call <$> generateOperand
    <|> pure Ret
    <|> Mov <$> generateOperand <*> generateOperand

generateOperand :: [Operand]
generateOperand =
  Immediate <$> generateImmediate
    <|> Register <$> generateRegister
    <|> Address <$> generateAddress

generateImmediate :: [Int64]
generateImmediate = [0, -1, 1, 256, 20000]

generateRegister :: [Register]
generateRegister = [minBound .. maxBound]

generateAddress :: [Address]
generateAddress =
  Address' <$> optional generateRegister <*> optional generateScaledRegister <*> generateDisplacement
  where
    generateScaledRegister = (,) <$> generateRegister <*> [minBound .. maxBound]
    generateDisplacement = [0, -1, 1, 256, 20000]
