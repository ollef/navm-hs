{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Target.X86.Assembler where

import Data.Bits
import Data.Int
import Data.Word
import Target.X86.Assembly
import Target.X86.MachineCode.Builder (Builder)
import qualified Target.X86.MachineCode.Builder as Builder

assembleInstructions :: [Instruction Register] -> Builder s
assembleInstructions =
  mconcat . map assembleInstruction

newtype Or a = Or {getOr :: a}
  deriving (Eq, Ord, Num, Bits)

instance Bits a => Semigroup (Or a) where
  Or a <> Or b = Or $ a .|. b

instance (Bits a, Num a) => Monoid (Or a) where
  mempty = 0

data Description s = Description
  { rex :: !(Maybe (Or Word8))
  , modRM :: !(Maybe (Or Word8))
  , sib :: !(Maybe (Or Word8))
  , displacement :: !(Builder s)
  , immediate :: !(Builder s)
  }

instance Semigroup (Description s) where
  Description rex1 modRM1 sib1 displacement1 immediate1 <> Description rex2 modRM2 sib2 displacement2 immediate2 =
    Description (rex1 <> rex2) (modRM1 <> modRM2) (sib1 <> sib2) (displacement1 <> displacement2) (immediate1 <> immediate2)

instance Monoid (Description s) where
  mempty = Description {rex = Nothing, modRM = Nothing, sib = Nothing, displacement = mempty, immediate = mempty}

flattenDescription :: Builder s -> Description s -> Builder s
flattenDescription opcode Description {..} =
  foldMap (Builder.word8 . (.|. 0b0100_0000) . getOr) rex
    <> opcode
    <> foldMap (Builder.word8 . getOr) modRM
    <> foldMap (Builder.word8 . getOr) sib
    <> displacement
    <> immediate

operandSize64 :: Description s
operandSize64 = mempty {rex = Just 0b1000}

modRMMod :: Word8 -> Description s
modRMMod bits = mempty {modRM = Just $ Or $ bits `shiftL` 6}

modRMRm :: Register -> Description s
modRMRm reg =
  mempty
    { rex = if rexBit == 0 then Nothing else Just $ Or rexBit
    , modRM = Just $ Or rmBits
    }
  where
    regWord = fromEnum8 reg
    rexBit = regWord `shiftR` 3
    rmBits = regWord .&. 0b111

modRMExt :: Word8 -> Description s
modRMExt ext =
  mempty {modRM = Just $ Or $ ext `shiftL` 3}

modRMReg :: Register -> Description s
modRMReg reg =
  (modRMExt regBits)
    { rex = if rexBit == 0 then Nothing else Just $ Or $ rexBit `shiftL` 2
    }
  where
    regWord = fromEnum8 reg
    rexBit = regWord `shiftR` 3
    regBits = regWord .&. 0b111

sibBase :: Register -> Description s
sibBase reg =
  mempty
    { rex = if rexBit == 0 then Nothing else Just $ Or rexBit
    , sib = Just $ Or baseBits
    }
  where
    regWord = fromEnum8 reg
    rexBit = regWord `shiftR` 3
    baseBits = regWord .&. 0b111

sibIndex :: Register -> Description s
sibIndex reg =
  mempty
    { rex = if rexBit == 0 then Nothing else Just $ Or $ rexBit `shiftL` 1
    , sib = Just $ Or $ indexBits `shiftL` 3
    }
  where
    regWord = fromEnum8 reg
    rexBit = regWord `shiftR` 3
    indexBits = regWord .&. 0b111

sibScale :: Scale -> Description s
sibScale scale =
  mempty {sib = if scaleWord == 0 then Nothing else Just $ Or $ scaleWord `shiftL` 6}
  where
    scaleWord = fromEnum8 scale

address :: Address Register -> Description s
address addr =
  modRMRm RSP
    <> case addr of
      Address (Just base) (Just (RSP, Scale1)) label displacement
        | base /= RSP ->
          address $ Address (Just RSP) (Just (base, Scale1)) label displacement
      Address Nothing Nothing Nothing displacement ->
        (modRMMod 0b00 <> sibBase RBP <> sibIndex RSP) {displacement = Builder.int32 displacement}
      Address (Just base) Nothing Nothing 0 ->
        modRMMod 0b00 <> sibBase base <> sibIndex RSP
      Address (Just base) (Just (index, scale)) Nothing 0
        | base /= RBP && base /= R13 ->
          modRMMod 0b00 <> sibBase base <> sibIndex index <> sibScale scale
      Address (Just base) (Just (index, scale)) Nothing (toImm8 -> Just displacement8) ->
        (modRMMod 0b01 <> sibBase base <> sibIndex index <> sibScale scale) {displacement = Builder.word8 displacement8}
      Address (Just base) Nothing Nothing (toImm8 -> Just displacment8) ->
        (modRMMod 0b01 <> sibBase base <> sibIndex RSP) {displacement = Builder.word8 displacment8}
      Address (Just base) (Just (index, scale)) Nothing displacement ->
        (modRMMod 0b10 <> sibBase base <> sibIndex index <> sibScale scale) {displacement = Builder.int32 displacement}
      Address (Just base) Nothing Nothing displacement ->
        (modRMMod 0b10 <> sibBase base <> sibIndex RSP) {displacement = Builder.int32 displacement}
      Address Nothing (Just (index, scale)) Nothing displacement ->
        (modRMMod 0b00 <> sibBase RBP <> sibIndex index <> sibScale scale) {displacement = Builder.int32 displacement}

assembleInstruction :: Instruction Register -> Builder s
assembleInstruction instruction =
  case instruction of
    Add (Register dst) _ (Register src) ->
      flattenDescription (Builder.word8 0x01) $
        operandSize64 <> modRMMod 0b11 <> modRMRm dst <> modRMReg src
    Add (Register dst) _ (Immediate (toImm8 -> Just imm8)) ->
      flattenDescription
        (Builder.word8 0x83)
        (operandSize64 <> modRMMod 0b11 <> modRMRm dst) {immediate = Builder.word8 imm8}
    Add (Register RAX) (Register RAX) (Immediate (toImm32 -> Just imm32)) ->
      flattenDescription
        (Builder.word8 0x05)
        operandSize64 {immediate = Builder.int32 imm32}
    Add (Register dst) (Register ((== dst) -> True)) (Immediate (toImm32 -> Just imm32)) ->
      flattenDescription
        (Builder.word8 0x81)
        (operandSize64 <> modRMMod 0b11 <> modRMRm dst) {immediate = Builder.int32 imm32}
    Add (Register _) _ (Immediate _) -> error "immediate operand has to fit in 32 bits"
    Add (Register dst) _ (Memory addr) ->
      flattenDescription (Builder.word8 0x03) $
        operandSize64 <> address addr <> modRMReg dst
    Add (Immediate _) _ _ -> error "immediate destination operand"
    Add (Memory addr) _ (Register src) ->
      flattenDescription (Builder.word8 0x01) $
        operandSize64 <> address addr <> modRMReg src
    Add (Memory addr) _ (Immediate (toImm8 -> Just imm8)) ->
      flattenDescription (Builder.word8 0x83) $
        (operandSize64 <> address addr) {immediate = Builder.word8 imm8}
    Add (Memory addr) _ (Immediate (toImm32 -> Just imm32)) ->
      flattenDescription (Builder.word8 0x81) $
        (operandSize64 <> address addr) {immediate = Builder.int32 imm32}
    Add (Memory _) _ (Immediate _) -> error "immediate operand has to fit in 32 bits"
    Add (Memory _) _ (Memory _) -> error "too many address operands"
    Mul (RDX, RAX) RAX (Register src) ->
      flattenDescription (Builder.word8 0xf7) $
        operandSize64 <> modRMExt 4 <> modRMMod 0b11 <> modRMRm src
    Mul (RDX, RAX) RAX (Memory addr) ->
      flattenDescription (Builder.word8 0xf7) $
        operandSize64 <> modRMExt 4 <> address addr
    Mul {} -> error "invalid mul operands"
    Call (Register r) ->
      flattenDescription (Builder.word8 0xff) $
        modRMMod 0b11 <> modRMExt 2 <> modRMRm r
    Call (Immediate _) -> Builder.word8 0xe8 <> Builder.int32 0
    Call (Memory addr) ->
      flattenDescription (Builder.word8 0xff) $
        address addr <> modRMExt 2
    Ret -> Builder.word8 0xc3 -- RET
    Mov (Register dst) (Immediate (toImm32 -> Just imm32)) ->
      flattenDescription
        (Builder.word8 0xc7)
        (operandSize64 <> modRMMod 0b11 <> modRMRm dst) {immediate = Builder.int32 imm32}
    Mov (Register r) (Immediate imm64) -> do
      let regWord = fromEnum8 r
          rexReg = regWord `shiftR` 3
          regOp = regWord .&. 0b111
      Builder.word8 (0x48 .|. rexReg) <> Builder.word8 (0xb8 .|. regOp) <> Builder.int64 imm64
    Mov (Register dst) (Register src) ->
      flattenDescription (Builder.word8 0x89) $
        operandSize64 <> modRMMod 0b11 <> modRMRm dst <> modRMReg src
    Mov (Register dst) (Memory addr) ->
      flattenDescription (Builder.word8 0x8b) $
        operandSize64 <> address addr <> modRMReg dst
    Mov (Memory addr) (Register src) ->
      flattenDescription (Builder.word8 0x89) $
        operandSize64 <> address addr <> modRMReg src
    Mov (Memory addr) (Immediate (toImm32 -> Just imm32)) ->
      flattenDescription (Builder.word8 0xc7) $
        (operandSize64 <> address addr) {immediate = Builder.int32 imm32}
    Mov (Memory _) (Immediate _) -> error "immediate operand has to fit in 32 bits"
    Mov (Memory _) (Memory _) -> error "too many memory operands"
    Mov (Immediate _) _ -> error "immediate destination operand"

toImm8 :: (Integral a, Bits a) => a -> Maybe Word8
toImm8 a = fromIntegral <$> (toIntegralSized a :: Maybe Int8)

toImm32 :: (Integral a, Bits a) => a -> Maybe Int32
toImm32 a = toIntegralSized a :: Maybe Int32

fromEnum8 :: Enum a => a -> Word8
fromEnum8 x =
  fromIntegral (fromEnum x)

type instance RegisterType (Builder s) = Register

instance FromInstruction (Builder s) where
  fromInstruction = assembleInstruction
