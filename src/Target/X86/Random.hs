module Target.X86.Random where

import Control.Applicative
import Data.Int
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Target.X86.Assembly

generateInstruction :: Gen (Instruction Register)
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

generateOperand :: Maybe (Operand Register) -> Gen (Operand Register)
generateOperand dst =
  Gen.choice $
    [ Immediate . Constant <$> generateImmediate
    , Register <$> generateRegister
    ]
      <> case dst of
        Just (Memory _) -> []
        _ -> [Memory <$> generateAddress]

generateRegisterOrAddressOperand :: Gen (Operand Register)
generateRegisterOrAddressOperand =
  Gen.choice
    [ Register <$> generateRegister
    , Memory <$> generateAddress
    ]

generateMovOperand :: Operand Register -> Gen (Operand Register)
generateMovOperand dst =
  Gen.choice $
    [ Immediate . Constant <$> generateMovImmediate dst
    , Register <$> generateRegister
    ]
      <> case dst of
        Memory _ -> []
        _ -> [Memory <$> generateAddress]

generateDestinationOperand :: Gen (Operand Register)
generateDestinationOperand =
  Gen.choice
    [ Register <$> generateRegister
    , Memory <$> generateAddress
    ]

generateImmediate :: Gen Int64
generateImmediate = fromIntegral <$> Gen.int32 Range.linearBounded

generateMovImmediate :: Operand Register -> Gen Int64
generateMovImmediate dst =
  case dst of
    Register _ -> Gen.int64 Range.linearBounded
    _ -> generateImmediate

generateRegister :: Gen Register
generateRegister = Gen.enumBounded

generateAddress :: Gen (Address Register)
generateAddress =
  Address
    <$> optional generateRegister
    <*> optional generateScaledIndexRegister
    <*> generateDisplacement
  where
    generateIndexRegister = Gen.filter (/= RSP) generateRegister
    generateScaledIndexRegister = (,) <$> generateIndexRegister <*> Gen.enumBounded
    generateDisplacement = Constant <$> Gen.int32 Range.linearBounded
