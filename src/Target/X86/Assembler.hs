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
import Label
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
  , opcode_ :: !(Or Word8)
  , modRM :: !(Maybe (Or Word8))
  , sib :: !(Maybe (Or Word8))
  , displacement_ :: !(Builder s)
  , immediate_ :: !(Builder s)
  }

instance Semigroup (Description s) where
  Description rex1 opcode1 modRM1 sib1 displacement1 immediate1 <> Description rex2 opcode2 modRM2 sib2 displacement2 immediate2 =
    Description (rex1 <> rex2) (opcode1 <> opcode2) (modRM1 <> modRM2) (sib1 <> sib2) (displacement1 <> displacement2) (immediate1 <> immediate2)

instance Monoid (Description s) where
  mempty = Description {rex = Nothing, opcode_ = mempty, modRM = Nothing, sib = Nothing, displacement_ = mempty, immediate_ = mempty}

flattenDescription :: Description s -> Builder s
flattenDescription Description {..} =
  foldMap (Builder.word8 . (.|. 0b0100_0000) . getOr) rex
    <> Builder.word8 (getOr opcode_)
    <> foldMap (Builder.word8 . getOr) modRM
    <> foldMap (Builder.word8 . getOr) sib
    <> displacement_
    <> immediate_

opcode :: Word8 -> Description s
opcode o = mempty {opcode_ = Or o}

opcodeReg :: Register -> Description s
opcodeReg reg =
  (opcode regBits) {rex = if rexBit == 0 then Nothing else Just $ Or rexBit}
  where
    regWord = fromEnum8 reg
    rexBit = regWord `shiftR` 3
    regBits = regWord .&. 0b111

immediate :: Builder s -> Description s
immediate i = mempty {immediate_ = i}

operandSize64 :: Description s
operandSize64 = mempty {rex = Just 0b1000}

modRMMod :: Word8 -> Description s
modRMMod bits = mempty {modRM = Just $ Or $ bits `shiftL` 6}

modRMRm :: Word8 -> Description s
modRMRm rm =
  mempty
    { rex = if rexBit == 0 then Nothing else Just $ Or rexBit
    , modRM = Just $ Or rmBits
    }
  where
    rexBit = rm `shiftR` 3
    rmBits = rm .&. 0b111

modRMRmReg :: Register -> Description s
modRMRmReg reg =
  modRMMod 0b11
    <> modRMRm (fromEnum8 reg)

modRMRmNone :: Description s
modRMRmNone = modRMRm 0b101

modRMRmSI :: Description s
modRMRmSI = modRMRm 0b100

-- | This is the /digit in opcode description, e.g. /4.
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

sibBaseNone :: Description s
sibBaseNone = sibBase RBP

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

sibIndexNone :: Description s
sibIndexNone = sibIndex RSP

sibScale :: Scale -> Description s
sibScale scale =
  mempty {sib = if scaleWord == 0 then Nothing else Just $ Or $ scaleWord `shiftL` 6}
  where
    scaleWord = fromEnum8 scale

address :: Int -> Address Register -> Description s
address offset addr =
  case addr of
    Address (Absolute (Just base) (Just (RSP, Scale1))) label disp
      | base /= RSP ->
        address offset $ Address (Absolute (Just RSP) (Just (base, Scale1))) label disp
    Address (Absolute Nothing Nothing) label disp ->
      modRMMod 0b00
        <> modRMRmSI
        <> sibBaseNone
        <> sibIndexNone
        <> immediate (Builder.int32 $ labelDisplacement label disp)
    Address (Absolute (Just base) Nothing) Nothing 0 ->
      modRMMod 0b00
        <> modRMRmSI
        <> sibBase base
        <> sibIndexNone
    Address (Absolute (Just base) (Just (index, scale))) Nothing 0
      | base /= RBP && base /= R13 ->
        modRMMod 0b00
          <> modRMRmSI
          <> sibBase base
          <> sibIndex index
          <> sibScale scale
    Address (Absolute (Just base) (Just (index, scale))) Nothing (toImm8 -> Just disp8) ->
      modRMMod 0b01 <> modRMRmSI
        <> sibBase base
        <> sibIndex index
        <> sibScale scale
        <> immediate (Builder.word8 disp8)
    Address (Absolute (Just base) Nothing) Nothing (toImm8 -> Just disp8) ->
      modRMMod 0b01
        <> modRMRmSI
        <> sibBase base
        <> sibIndexNone
        <> immediate (Builder.word8 disp8)
    Address (Absolute (Just base) (Just (index, scale))) label disp ->
      modRMMod 0b10
        <> modRMRmSI
        <> sibBase base
        <> sibIndex index
        <> sibScale scale
        <> immediate (Builder.int32 $ labelDisplacement label disp)
    Address (Absolute (Just base) Nothing) label disp ->
      modRMMod 0b10
        <> modRMRmSI
        <> sibBase base
        <> sibIndexNone
        <> immediate (Builder.int32 $labelDisplacement label disp)
    Address (Absolute Nothing (Just (index, scale))) label disp ->
      modRMMod 0b00
        <> modRMRmSI
        <> sibBaseNone
        <> sibIndex index
        <> sibScale scale
        <> immediate (Builder.int32 $ labelDisplacement label disp)
    Address Relative (Just label) disp ->
      modRMMod 0b00
        <> modRMRmNone
        <> immediate (Builder.useRelativeToEnd label Builder.Int32 (fromIntegral disp - offset))
    Address Relative Nothing disp ->
      modRMRmNone
        <> immediate (Builder.int32 disp)
  where
    labelDisplacement :: Maybe Label -> Int32 -> Int32
    labelDisplacement Nothing i = i
    -- TODO relocation
    labelDisplacement (Just _) _ = 0

assembleInstruction :: Instruction Register -> Builder s
assembleInstruction instruction =
  case instruction of
    Add (Register dst) _ (Register src) ->
      flattenDescription $
        operandSize64
          <> opcode 0x01
          <> modRMRmReg dst
          <> modRMReg src
    Add (Register dst) _ (Immediate (toImm8 -> Just imm8)) ->
      flattenDescription $
        operandSize64
          <> opcode 0x83
          <> modRMRmReg dst
          <> immediate (Builder.word8 imm8)
    Add (Register RAX) (Register RAX) (Immediate imm32) ->
      flattenDescription $
        operandSize64
          <> opcode 0x05
          <> immediate (Builder.int32 imm32)
    Add (Register dst) _ (Immediate imm32) ->
      flattenDescription $
        operandSize64
          <> opcode 0x81
          <> modRMRmReg dst
          <> immediate (Builder.int32 imm32)
    Add (Register dst) _ (Memory addr) ->
      flattenDescription $
        operandSize64
          <> opcode 0x03
          <> address 0 addr
          <> modRMReg dst
    Add (Immediate _) _ _ -> error "immediate destination operand"
    Add (Memory addr) _ (Register src) ->
      flattenDescription $
        operandSize64
          <> opcode 0x01
          <> address 0 addr
          <> modRMReg src
    Add (Memory addr) _ (Immediate (toImm8 -> Just imm8)) ->
      flattenDescription $
        operandSize64
          <> opcode 0x83
          <> address 1 addr
          <> immediate (Builder.word8 imm8)
    Add (Memory addr) _ (Immediate imm32) ->
      flattenDescription $
        operandSize64
          <> opcode 0x81
          <> address 4 addr
          <> immediate (Builder.int32 imm32)
    Add (Memory _) _ (Memory _) -> error "too many address operands"
    Mul (RDX, RAX) RAX (Register src) ->
      flattenDescription $
        operandSize64
          <> opcode 0xf7
          <> modRMExt 4
          <> modRMRmReg src
    Mul (RDX, RAX) RAX (Memory addr) ->
      flattenDescription $
        operandSize64
          <> opcode 0xf7
          <> modRMExt 4
          <> address 0 addr
    Mul {} -> error "invalid mul operands"
    Jmp (JmpRelative (Just label) offset) ->
      Builder.flexible
        (flattenDescription $ opcode 0xeb <> immediate (Builder.useRelativeToEnd label Builder.Int8 (fromIntegral offset)))
        (flattenDescription $ opcode 0xe9 <> immediate (Builder.useRelativeToEnd label Builder.Int32 (fromIntegral offset)))
    Jmp (JmpRelative Nothing (toImm8 . subtract 2 -> Just imm8)) ->
      flattenDescription $
        opcode 0xeb
          <> immediate (Builder.word8 imm8)
    Jmp (JmpRelative Nothing imm32) ->
      flattenDescription $
        opcode 0xe9
          <> immediate (Builder.int32 $ imm32 - 5)
    Jmp (JmpAbsolute (Register r)) ->
      flattenDescription $
        opcode 0xff
          <> modRMExt 4
          <> modRMRmReg r
    Jmp (JmpAbsolute (Immediate _)) ->
      -- TODO relocation
      flattenDescription $
        opcode 0xe9
          <> immediate (Builder.int32 0)
    Jmp (JmpAbsolute (Memory addr)) ->
      flattenDescription $
        opcode 0xff
          <> modRMExt 4
          <> address 0 addr
    Call (Register r) ->
      flattenDescription $
        opcode 0xff
          <> modRMExt 2
          <> modRMRmReg r
    Call (Immediate _) ->
      flattenDescription $
        opcode 0xe8
          <> immediate (Builder.int32 0)
    Call (Memory addr) ->
      flattenDescription $
        opcode 0xff
          <> address 0 addr
          <> modRMExt 2
    Ret -> Builder.word8 0xc3 -- RET
    MovImmediate64 dst (toImm32 -> Just imm32) ->
      assembleInstruction $ Mov (Register dst) (Immediate imm32)
    MovImmediate64 dst imm64 -> do
      flattenDescription $
        operandSize64
          <> opcode 0xb8
          <> opcodeReg dst
          <> immediate (Builder.int64 imm64)
    Mov (Register dst) (Immediate imm32) ->
      flattenDescription $
        operandSize64
          <> opcode 0xc7
          <> modRMRmReg dst
          <> immediate (Builder.int32 imm32)
    Mov (Register dst) (Register src) ->
      flattenDescription $
        operandSize64
          <> opcode 0x89
          <> modRMRmReg dst
          <> modRMReg src
    Mov (Register dst) (Memory addr) ->
      flattenDescription $
        operandSize64
          <> opcode 0x8b
          <> address 0 addr
          <> modRMReg dst
    Mov (Memory addr) (Register src) ->
      flattenDescription $
        operandSize64
          <> opcode 0x89
          <> address 0 addr
          <> modRMReg src
    Mov (Memory addr) (Immediate imm32) ->
      flattenDescription $
        operandSize64
          <> opcode 0xc7
          <> address 4 addr
          <> immediate (Builder.int32 imm32)
    Mov (Memory _) (Memory _) -> error "too many memory operands"
    Mov (Immediate _) _ -> error "immediate destination operand"
    Define label -> Builder.define label

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
