{-# LANGUAGE OverloadedStrings #-}

module Target.X86.Random where

import Control.Applicative
import Control.Monad
import Data.Int
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Label (Label)
import Target.X86.Assembly

generateInstructions :: [Label] -> Gen [Instruction Register]
generateInstructions labels = go labels
  where
    go [] = instructionList
    go (undefinedLabel : undefinedLabels) = (\ls1 ls2 -> Define undefinedLabel : ls1 <> ls2) <$> instructionList <*> go undefinedLabels
    instructionList = Gen.list (Range.linear 1 100) $ generateInstruction labels

generateInstruction :: [Label] -> Gen (Instruction Register)
generateInstruction labels =
  Gen.choice
    [ do
        dst <- generateDestinationOperand labels
        src <- generateOperand labels $ Just dst
        pure $ Add dst dst src
    , Mul (RDX, RAX) RAX <$> generateRegisterOrAddressOperand labels
    , Jmp <$> generateJmpOperand labels
    , Call <$> generateOperand labels Nothing
    , pure Ret
    , do
        dst <- generateDestinationOperand labels
        src <- generateMovOperand labels dst
        pure $ Mov dst src
    ]

generateOperand :: [Label] -> Maybe (Operand Register) -> Gen (Operand Register)
generateOperand labels dst =
  Gen.choice $
    [ Immediate <$> generateImmediate
    , Register <$> generateRegister
    ]
      <> case dst of
        Just (Memory _) -> []
        _ -> [Memory <$> generateAddress labels]

generateJmpOperand :: [Label] -> Gen (JmpOperand Register)
generateJmpOperand labels =
  Gen.choice
    [ JmpRelative <$> generateOptionalLabel labels <*> Gen.int32 Range.linearBounded
    , JmpAbsolute <$> generateOperand labels Nothing
    ]

generateRegisterOrAddressOperand :: [Label] -> Gen (Operand Register)
generateRegisterOrAddressOperand labels =
  Gen.choice
    [ Register <$> generateRegister
    , Memory <$> generateAddress labels
    ]

generateMovOperand :: [Label] -> Operand Register -> Gen (Operand Register)
generateMovOperand labels dst =
  Gen.choice $
    [ Immediate <$> generateMovImmediate dst
    , Register <$> generateRegister
    ]
      <> case dst of
        Memory _ -> []
        _ -> [Memory <$> generateAddress labels]

generateDestinationOperand :: [Label] -> Gen (Operand Register)
generateDestinationOperand labels =
  Gen.choice
    [ Register <$> generateRegister
    , Memory <$> generateAddress labels
    ]

generateImmediate :: Gen Int64
generateImmediate =
  fromIntegral <$> Gen.int32 Range.linearBounded

generateMovImmediate :: Operand Register -> Gen Int64
generateMovImmediate dst =
  case dst of
    Register _ -> Gen.int64 Range.linearBounded
    _ -> generateImmediate

generateLabel :: Alternative f => [Label] -> f (Gen Label)
generateLabel [] = empty
generateLabel labels = pure $ Gen.element labels

generateOptionalLabel :: [Label] -> Gen (Maybe Label)
generateOptionalLabel labels = fmap join $ optional $ sequence $ generateLabel labels

generateRegister :: Gen Register
generateRegister = Gen.enumBounded

generateAddress :: [Label] -> Gen (Address Register)
generateAddress labels =
  Address
    <$> generateBase
    <*> generateOptionalLabel labels
    <*> generateDisplacement
  where
    generateDisplacement = Gen.int32 Range.linearBounded

generateBase :: Gen (Base Register)
generateBase =
  Gen.choice
    [ Absolute <$> optional generateRegister <*> optional generateScaledIndexRegister
    , pure Relative
    ]
  where
    generateScaledIndexRegister = (,) <$> generateIndexRegister <*> Gen.enumBounded
    generateIndexRegister = Gen.filter (/= RSP) generateRegister
