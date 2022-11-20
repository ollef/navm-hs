{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Target.X86.Random where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import qualified Data.BitSet as BitSet
import Data.Int
import Data.List (sort)
import Data.String
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Label (Label)
import qualified Register
import Target.X86.Assembly
import Target.X86.Constraints
import qualified Target.X86.Register as Register

generateInstructions :: Gen [Instruction Register]
generateInstructions = do
  labels <- Gen.subsequence [fromString $ pure c | c <- ['a' .. 'z']]
  instructions <- Gen.list (Range.linear 1 10) $ fillInRegisters =<< generateInstruction labels
  labelPositions <- Gen.list (Range.singleton $ length labels) $ Gen.int $ Range.constant 0 $ length instructions
  pure $ defineLabels 0 (zip labels $ sort labelPositions) instructions
  where
    defineLabels _pos [] is = is
    defineLabels _pos ls [] = Define . fst <$> ls
    defineLabels pos ls@((l, lpos) : ls') is@(i : is')
      | pos >= lpos = Define l : defineLabels pos ls' is
      | otherwise = i : defineLabels (pos + 1) ls is'
    fillInRegisters =
      constrain
        Constrainers
          { registerOccurrence = \_ class_ ~() -> generateRegister class_
          , forceSame = \(Destination dst) _ -> pure (Destination dst, Source dst)
          , forceRegister = \class_ _ -> Source . Register <$> generateRegister class_
          }

generateSSAInstructions :: (Instruction Register.Virtual -> Bool) -> Gen [Instruction Register.Virtual]
generateSSAInstructions predicate = do
  instructions <- Gen.list (Range.linear 1 1000) $ fillInRegisters =<< generateInstruction []
  let instructions' = evalState (mapM (mapMWithClass go) $ filter predicate instructions) 1
  pure $ mov (Register $ Register.V 0) (Immediate 0) : instructions'
  where
    fillInRegisters = mapMWithClass \occurrence _ ~() ->
      case occurrence of
        Definition -> pure $ Register.V 0
        Use -> Register.V <$> Gen.int Range.constantBounded
    go occurrence _ (Register.V register) =
      case occurrence of
        Definition -> do
          !nextRegister <- get
          put $ nextRegister + 1
          pure $ Register.V nextRegister
        Use -> do
          !nextRegister <- get
          pure $ Register.V $ abs register `mod` nextRegister

generateInstruction :: [Label] -> Gen (Instruction ())
generateInstruction labels =
  Gen.choice
    [ Add <$> generateDestinationOperand labels <*> generateOperand labels <*> generateOperand labels
    , Mul ((), ()) () <$> generateRegisterOrAddressOperand labels
    , Jmp <$> generateJmpOperand labels
    , Call <$> generateOperand labels
    , pure Ret
    , Int <$> Gen.word8 Range.linearBounded
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
