{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Target.X86.Assembler where

import Data.Bits
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as Lazy
import Data.Int
import Data.Word
import Target.X86.Assembly
import Text.Printf (printf)

newtype MachineCode = MachineCode Lazy.ByteString
  deriving (Eq)

instance Show MachineCode where
  showsPrec _ (MachineCode lbs) =
    Lazy.foldr (\c -> (showString (printf "%02x " c) .)) id lbs

newtype MachineCodeBuilder = MachineCodeBuilder Builder
  deriving (Semigroup, Monoid)

buildMachineCode :: MachineCodeBuilder -> MachineCode
buildMachineCode (MachineCodeBuilder b) = MachineCode $ Builder.toLazyByteString b

assembleInstructions :: [Instruction] -> MachineCodeBuilder
assembleInstructions =
  mconcat . map assembleInstruction

newtype Or a = Or {getOr :: a}
  deriving (Eq, Ord, Num, Bits)

instance Bits a => Semigroup (Or a) where
  Or a <> Or b = Or $ a .|. b

instance (Bits a, Num a) => Monoid (Or a) where
  mempty = 0

data Description = Description
  { rex :: !(Maybe (Or Word8))
  , modRM :: !(Maybe (Or Word8))
  , sib :: !(Maybe (Or Word8))
  , displacement :: !MachineCodeBuilder
  , immediate :: !MachineCodeBuilder
  }

instance Semigroup Description where
  Description rex1 modRM1 sib1 displacement1 immediate1 <> Description rex2 modRM2 sib2 displacement2 immediate2 =
    Description (rex1 <> rex2) (modRM1 <> modRM2) (sib1 <> sib2) (displacement1 <> displacement2) (immediate1 <> immediate2)

instance Monoid Description where
  mempty = Description {rex = Nothing, modRM = Nothing, sib = Nothing, displacement = mempty, immediate = mempty}

flattenDescription :: MachineCodeBuilder -> Description -> MachineCodeBuilder
flattenDescription opcode Description {..} =
  foldMap (word8 . (.|. 0b0100_0000) . getOr) rex
    <> opcode
    <> foldMap (word8 . getOr) modRM
    <> foldMap (word8 . getOr) sib
    <> displacement
    <> immediate

operandSize64 :: Description
operandSize64 = mempty {rex = Just 0b1000}

modRMMod :: Word8 -> Description
modRMMod bits = mempty {modRM = Just $ Or $ bits `shiftL` 6}

modRMRm :: Register -> Description
modRMRm reg =
  mempty
    { rex = if rexBit == 0 then Nothing else Just $ Or rexBit
    , modRM = Just $ Or rmBits
    }
  where
    regWord = fromEnum8 reg
    rexBit = regWord `shiftR` 3
    rmBits = regWord .&. 0b111

modRMExt :: Word8 -> Description
modRMExt ext =
  mempty {modRM = Just $ Or $ ext `shiftL` 3}

modRMReg :: Register -> Description
modRMReg reg =
  (modRMExt regBits)
    { rex = if rexBit == 0 then Nothing else Just $ Or $ rexBit `shiftL` 2
    }
  where
    regWord = fromEnum8 reg
    rexBit = regWord `shiftR` 3
    regBits = regWord .&. 0b111

sibBase :: Register -> Description
sibBase reg =
  mempty
    { rex = if rexBit == 0 then Nothing else Just $ Or rexBit
    , sib = Just $ Or baseBits
    }
  where
    regWord = fromEnum8 reg
    rexBit = regWord `shiftR` 3
    baseBits = regWord .&. 0b111

sibIndex :: Register -> Description
sibIndex reg =
  mempty
    { rex = if rexBit == 0 then Nothing else Just $ Or $ rexBit `shiftL` 1
    , sib = Just $ Or $ indexBits `shiftL` 3
    }
  where
    regWord = fromEnum8 reg
    rexBit = regWord `shiftR` 3
    indexBits = regWord .&. 0b111

sibScale :: Scale -> Description
sibScale scale =
  mempty {sib = if scaleWord == 0 then Nothing else Just $ Or $ scaleWord `shiftL` 6}
  where
    scaleWord = fromEnum8 scale

address :: Address -> Description
address (Address' (Just base) (Just (RSP, Scale1)) displacement)
  | base /= RSP =
    address $ Address' (Just RSP) (Just (base, Scale1)) displacement
address (Address' Nothing Nothing displacement) =
  (modRMMod 0b00 <> sibBase RBP <> sibIndex RSP) {displacement = int32 displacement}
address (Address' (Just base) Nothing 0) =
  modRMMod 0b00 <> sibBase base <> sibIndex RSP
address (Address' (Just base) (Just (index, scale)) 0) =
  modRMMod 0b00 <> sibBase base <> sibIndex index <> sibScale scale
address (Address' (Just base) (Just (index, scale)) (toImm8 -> Just displacement8)) =
  (modRMMod 0b01 <> sibBase base <> sibIndex index <> sibScale scale) {displacement = word8 displacement8}
address (Address' (Just base) Nothing (toImm8 -> Just displacment8)) =
  (modRMMod 0b01 <> sibBase base <> sibIndex RSP) {displacement = word8 displacment8}
address (Address' (Just base) (Just (index, scale)) displacement) =
  (modRMMod 0b10 <> sibBase base <> sibIndex index <> sibScale scale) {displacement = int32 displacement}
address (Address' (Just base) Nothing displacement) =
  (modRMMod 0b10 <> sibBase base <> sibIndex RSP) {displacement = int32 displacement}
address (Address' Nothing (Just (index, scale)) displacement) =
  (modRMMod 0b00 <> sibBase RBP <> sibIndex index <> sibScale scale) {displacement = int32 displacement}

assembleInstruction :: Instruction -> MachineCodeBuilder
assembleInstruction instruction =
  case instruction of
    Add (Register dst) (Register src) ->
      flattenDescription (word8 0x01) $
        operandSize64 <> modRMMod 0b11 <> modRMRm dst <> modRMReg src
    Add (Register dst) (Immediate (toImm8 -> Just imm8)) ->
      flattenDescription
        (word8 0x83)
        (operandSize64 <> modRMMod 0b11 <> modRMRm dst) {immediate = word8 imm8}
    Add (Register RAX) (Immediate (toImm32 -> Just imm32)) ->
      flattenDescription
        (word8 0x05)
        operandSize64 {immediate = int32 imm32}
    Add (Register dst) (Immediate (toImm32 -> Just imm32)) ->
      flattenDescription
        (word8 0x81)
        (operandSize64 <> modRMMod 0b11 <> modRMRm dst) {immediate = int32 imm32}
    Add (Register _) (Immediate _) -> error "immediate operand has to fit in 32 bits"
    Add (Register dst) (Address addr) ->
      flattenDescription (word8 0x03) $
        operandSize64 <> modRMRm RSP <> address addr <> modRMReg dst
    Add (Immediate _) _ -> error "immediate destination operand"
    Add (Address addr) (Register src) ->
      flattenDescription (word8 0x01) $
        operandSize64 <> modRMRm RSP <> address addr <> modRMReg src
    Add (Address addr) (Immediate (toImm8 -> Just imm8)) ->
      flattenDescription (word8 0x83) $
        (operandSize64 <> modRMRm RSP <> address addr) {immediate = word8 imm8}
    Add (Address addr) (Immediate (toImm32 -> Just imm32)) ->
      flattenDescription (word8 0x81) $
        (operandSize64 <> modRMRm RSP <> address addr) {immediate = int32 imm32}
    Add _ _ -> mempty
    Call (Register r) ->
      flattenDescription (word8 0xff) $
        modRMMod 0b11 <> modRMExt 2 <> modRMRm r
    Call (Immediate _) -> word8 0xe8 <> int32 0
    Call (Address addr) ->
      flattenDescription (word8 0xff) $
        address addr <> modRMExt 2 <> modRMRm RSP
    Ret -> word8 0xc3 -- RET
    Mov (Register dst) (Immediate (toImm32 -> Just imm32)) ->
      flattenDescription
        (word8 0xc7)
        (operandSize64 <> modRMMod 0b11 <> modRMRm dst) {immediate = int32 imm32}
    Mov (Register r) (Immediate imm64) -> do
      let regWord = fromEnum8 r
          rexReg = regWord `shiftR` 3
          regOp = regWord .&. 0b111
      word8 (0x48 .|. rexReg) <> word8 (0xb8 .|. regOp) <> int64 imm64
    Mov (Register dst) (Register src) ->
      flattenDescription (word8 0x89) $
        operandSize64 <> modRMMod 0b11 <> modRMRm dst <> modRMReg src
    Mov (Register dst) (Address addr) ->
      flattenDescription (word8 0x8b) $
        operandSize64 <> modRMRm RSP <> address addr <> modRMReg dst
    Mov (Address addr) (Register src) ->
      flattenDescription (word8 0x89) $
        operandSize64 <> modRMRm RSP <> address addr <> modRMReg src
    Mov (Address addr) (Immediate (toImm32 -> Just imm32)) ->
      flattenDescription (word8 0xc7) $
        (operandSize64 <> modRMRm RSP <> address addr) {immediate = int32 imm32}
    Mov (Address _) (Address _) -> error "too many memory operands for mov"
    Mov _ _ -> mempty

toImm8 :: (Integral a, Bits a) => a -> Maybe Word8
toImm8 a = fromIntegral <$> (toIntegralSized a :: Maybe Int8)

toImm32 :: (Integral a, Bits a) => a -> Maybe Int32
toImm32 a = toIntegralSized a :: Maybe Int32

word8 :: Word8 -> MachineCodeBuilder
word8 = MachineCodeBuilder . Builder.word8

int32 :: Int32 -> MachineCodeBuilder
int32 = MachineCodeBuilder . Builder.int32LE

int64 :: Int64 -> MachineCodeBuilder
int64 = MachineCodeBuilder . Builder.int64LE

fromEnum8 :: Enum a => a -> Word8
fromEnum8 x =
  fromIntegral (fromEnum x)

instance FromInstruction MachineCodeBuilder where
  fromInstruction = assembleInstruction
