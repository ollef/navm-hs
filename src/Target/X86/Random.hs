module Target.X86.Random where

import Control.Applicative
import Data.Int
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Target.X86.Assembly

generateInstruction :: Gen Instruction
generateInstruction =
  Gen.choice
    [ do
        dst <- generateDestinationOperand
        src <- generateOperand $ Just dst
        pure $ Add dst src
    , Call <$> generateOperand Nothing
    , pure Ret
    , do
        dst <- generateDestinationOperand
        src <- generateMovOperand dst
        pure $ Mov dst src
    ]

generateOperand :: Maybe Operand -> Gen Operand
generateOperand dst =
  Gen.choice
    [ Immediate <$> generateImmediate
    , Register <$> generateRegister
    , case dst of
        Just (Address _) -> empty
        _ -> Address <$> generateAddress
    ]

generateMovOperand :: Operand -> Gen Operand
generateMovOperand dst =
  Gen.choice
    [ Immediate <$> generateMovImmediate
    , Register <$> generateRegister
    , case dst of
        Address _ -> empty
        _ -> Address <$> generateAddress
    ]

generateDestinationOperand :: Gen Operand
generateDestinationOperand =
  Gen.choice
    [ Register <$> generateRegister
    , Address <$> generateAddress
    ]

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
