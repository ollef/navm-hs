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
        pure $ Add dst dst src
    , Mul (RDX, RAX) RAX <$> generateRegisterOrAddressOperand
    , Call <$> generateOperand Nothing
    , pure Ret
    , do
        dst <- generateDestinationOperand
        src <- generateMovOperand dst
        pure $ Mov dst src
    ]

generateOperand :: Maybe Operand -> Gen Operand
generateOperand dst =
  Gen.choice $
    [ Immediate <$> generateImmediate
    , Register <$> generateRegister
    ]
      <> case dst of
        Just (Address _) -> []
        _ -> [Address <$> generateAddress]

generateRegisterOrAddressOperand :: Gen Operand
generateRegisterOrAddressOperand =
  Gen.choice
    [ Register <$> generateRegister
    , Address <$> generateAddress
    ]

generateMovOperand :: Operand -> Gen Operand
generateMovOperand dst =
  Gen.choice $
    [ Immediate <$> generateMovImmediate dst
    , Register <$> generateRegister
    ]
      <> case dst of
        Address _ -> []
        _ -> [Address <$> generateAddress]

generateDestinationOperand :: Gen Operand
generateDestinationOperand =
  Gen.choice
    [ Register <$> generateRegister
    , Address <$> generateAddress
    ]

generateImmediate :: Gen Int64
generateImmediate = fromIntegral <$> Gen.int32 Range.linearBounded

generateMovImmediate :: Operand -> Gen Int64
generateMovImmediate dst =
  case dst of
    Register _ -> Gen.int64 Range.linearBounded
    _ -> generateImmediate

generateRegister :: Gen Register
generateRegister = Gen.enumBounded

generateAddress :: Gen Address
generateAddress =
  Address'
    <$> optional generateRegister
    <*> optional generateScaledIndexRegister
    <*> generateDisplacement
  where
    generateScaledIndexRegister = (,) <$> generateIndexRegister <*> Gen.enumBounded
    generateDisplacement = Gen.int32 Range.linearBounded
    generateIndexRegister = Gen.filter (/= RSP) generateRegister
