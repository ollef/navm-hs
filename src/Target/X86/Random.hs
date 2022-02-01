{-# LANGUAGE OverloadedStrings #-}

module Target.X86.Random where

import Control.Applicative
import Data.Int
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Label (Label)
import Target.X86.Assembly

generateInstruction :: [Label] -> Gen (Instruction Register)
generateInstruction labels =
  Gen.choice
    [ do
        dst <- generateDestinationOperand
        src <- generateOperand labels $ Just dst
        pure $ Add dst dst src
    , Mul (RDX, RAX) RAX <$> generateRegisterOrAddressOperand
    , Call <$> generateOperand labels Nothing
    , pure Ret
    , do
        dst <- generateDestinationOperand
        src <- generateMovOperand labels dst
        pure $ Mov dst src
    ]

generateOperand :: [Label] -> Maybe (Operand Register) -> Gen (Operand Register)
generateOperand labels dst =
  Gen.choice $
    [ Immediate <$> generateImmediate labels
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

generateMovOperand :: [Label] -> Operand Register -> Gen (Operand Register)
generateMovOperand labels dst =
  Gen.choice $
    [ Immediate <$> generateMovImmediate labels dst
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

generateImmediate :: Num n => [Label] -> Gen (Immediate n)
generateImmediate labels =
  Gen.choice $
    [ Constant . fromIntegral <$> Gen.int32 Range.linearBounded
    ]
      <> (fmap Label <$> generateLabel labels)

generateMovImmediate :: [Label] -> Operand Register -> Gen (Immediate Int64)
generateMovImmediate labels dst =
  case dst of
    Register _ ->
      Gen.choice $
        [Constant <$> Gen.int64 Range.linearBounded]
          <> (fmap Label <$> generateLabel labels)
    _ -> generateImmediate labels

generateLabel :: [Label] -> [Gen Label]
generateLabel [] = []
generateLabel labels = [Gen.element labels]

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
