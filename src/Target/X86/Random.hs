{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Target.X86.Random where

import Control.Applicative
import Control.Monad
import qualified Data.BitSet as BitSet
import Data.Int
import Data.List (sort)
import Data.String
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Label (Label)
import Target.X86.Assembly
import Target.X86.Constraints
import qualified Target.X86.Register as Register

generateInstructions :: Gen [Instruction Register]
generateInstructions = do
  labels <- Gen.subsequence [fromString $ pure c | c <- ['a' .. 'z']]
  instructions <- Gen.list (Range.linear 1 1000) $ generateInstruction labels
  instructions' <-
    mapM
      ( constrain
          Constrainers
            { registerOccurrence = \_ class_ () -> generateRegister class_
            , forceSame = \(Destination dst) _ -> pure (Destination dst, Source dst)
            , forceRegister = \class_ _ -> Source . Register <$> generateRegister class_
            }
      )
      instructions
  labelPositions <- Gen.list (Range.singleton $ length labels) $ Gen.int $ Range.constant 0 $ length instructions'
  pure $ defineLabels 0 (zip labels $ sort labelPositions) instructions'
  where
    defineLabels _pos [] is = is
    defineLabels _pos ls [] = Define . fst <$> ls
    defineLabels pos ls@((l, lpos) : ls') is@(i : is')
      | pos >= lpos = Define l : defineLabels pos ls' is
      | otherwise = i : defineLabels (pos + 1) ls is'

generateInstruction :: [Label] -> Gen (Instruction ())
generateInstruction labels =
  Gen.choice
    [ Add <$> generateDestinationOperand labels <*> generateOperand labels <*> generateOperand labels
    , Mul ((), ()) () <$> generateRegisterOrAddressOperand labels
    , Jmp <$> generateJmpOperand labels
    , Call <$> generateOperand labels
    , pure Ret
    , Mov <$> generateDestinationOperand labels <*> generateOperand labels
    , MovImmediate64 () <$> Gen.int64 Range.linearBounded
    ]

generateDestinationOperand :: [Label] -> Gen (Operand ())
generateDestinationOperand labels =
  Gen.choice
    [ pure $ Register ()
    , Memory <$> generateAddress labels
    ]

generateOperand :: [Label] -> Gen (Operand ())
generateOperand labels =
  Gen.choice
    [ Immediate <$> generateImmediate
    , pure $ Register ()
    , Memory <$> generateAddress labels
    ]

generateJmpOperand :: [Label] -> Gen (JmpOperand ())
generateJmpOperand labels =
  Gen.choice
    [ JmpRelative <$> generateOptionalLabel labels <*> generateImmediate
    , JmpAbsolute <$> generateOperand labels
    ]

generateRegisterOrAddressOperand :: [Label] -> Gen (Operand ())
generateRegisterOrAddressOperand labels =
  Gen.choice
    [ pure $ Register ()
    , Memory <$> generateAddress labels
    ]

generateImmediate :: Gen Int32
generateImmediate =
  Gen.int32 Range.linearBounded

generateLabel :: Alternative f => [Label] -> f (Gen Label)
generateLabel [] = empty
generateLabel labels = pure $ Gen.element labels

generateOptionalLabel :: [Label] -> Gen (Maybe Label)
generateOptionalLabel labels = fmap join $ optional $ sequence $ generateLabel labels

generateRegister :: Register.Class -> Gen Register
generateRegister = Gen.element . BitSet.toList

generateAddress :: [Label] -> Gen (Address ())
generateAddress labels =
  Address
    <$> generateBase
    <*> generateOptionalLabel labels
    <*> generateDisplacement
  where
    generateDisplacement = generateImmediate

generateBase :: Gen (Base ())
generateBase =
  Gen.choice
    [ Absolute <$> optional (pure ()) <*> optional generateScaledIndexRegister
    , pure Relative
    ]
  where
    generateScaledIndexRegister = (,) () <$> Gen.enumBounded
