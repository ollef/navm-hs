{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}

module Target.X86.Assembler where

import Data.Bifunctor
import Data.Bits
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as Lazy
import Data.Int
import Data.Maybe
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

assembleInstruction :: Instruction -> MachineCodeBuilder
assembleInstruction instruction =
  case instruction of
    Add (Register dstReg) (Register srcReg) ->
      rexMod (word8 0x01) dstReg (Just srcReg)
    Add (Register r) (Immediate (toImm8 -> Just imm8)) ->
      rexMod (word8 0x83) r Nothing <> word8 imm8
    Add (Register RAX) (Immediate (toImm32 -> Just imm32)) ->
      word8 0x48 -- REX prefix
        <> word8 0x05 -- ADD
        <> int32 imm32
    Add (Register r) (Immediate (toImm32 -> Just imm32)) ->
      rexMod (word8 0x81) r Nothing <> int32 imm32
    Add (Register _) (Immediate _) -> error "immediate operand has to fit in 32 bits"
    Add (Immediate _) _ -> error "immediate destination operand"
    Add (Address addr) (Register r) ->
      word8 0x48
        <> word8 0x83
        <> word8 0x04
        <> word8 0
        <> word8 0
    Add (Register r) (Address addr) ->
      word8 0x48
        <> word8 0x03
        <> word8 0x04
        <> word8 (sib addr)
    Add _ _ -> mempty
    Call (Register r) -> do
      let regWord = fromEnum8 r
          rexReg = regWord `shiftR` 3
          regOp = regWord .&. 0b111
      if rexReg == 0
        then
          word8 0xff
            <> word8 (0xd0 .|. regOp) -- CALL
        else
          word8 (0x41 .|. rexReg) -- REX prefix
            <> word8 0xff -- CALL
            <> word8 (0xd0 .|. regOp)
    Call (Immediate _) -> word8 0xe8 <> int32 0
    Call _ -> mempty
    Ret -> word8 0xc3 -- RET
    Mov (Register r) (Immediate (toImm32 -> Just imm32)) ->
      rexMod (word8 0xc7) r Nothing <> int32 imm32
    Mov (Register r) (Immediate imm64) -> do
      let regWord = fromEnum8 r
          rexReg = regWord `shiftR` 3
          regOp = regWord .&. 0b111
      word8 (0x48 .|. rexReg) <> word8 (0xb8 .|. regOp) <> int64 imm64
    Mov (Register dstReg) (Register srcReg) ->
      rexMod (word8 0x89) dstReg (Just srcReg)
    Mov (Address _) (Address _) -> error "too many memory operands for mov"
    Mov _ _ -> mempty
  where
    toImm8 :: (Integral a, Bits a) => a -> Maybe Word8
    toImm8 a = fromIntegral <$> (toIntegralSized a :: Maybe Int8)

    toImm32 :: (Integral a, Bits a) => a -> Maybe Int32
    toImm32 a = toIntegralSized a :: Maybe Int32

    rexMod ::
      MachineCodeBuilder ->
      Register ->
      Maybe Register ->
      MachineCodeBuilder
    rexMod opcode dstReg (fromMaybe RAX -> srcReg) = do
      let dstRegWord = fromEnum8 dstReg
          dstRexReg = dstRegWord `shiftR` 3
          dstRegMod = dstRegWord .&. 0b111
      let srcRegWord = fromEnum8 srcReg
          srcRexReg = srcRegWord `shiftR` 3
          srcRegMod = srcRegWord .&. 0b111
      word8 (0x48 .|. dstRexReg .|. (srcRexReg `shiftL` 2)) -- REX prefix
        <> opcode
        <> word8 (0xc0 .|. dstRegMod .|. (srcRegMod `shiftL` 3)) -- Mod R/M
    sib :: Address -> Word8
    sib (Address' base index displacement) = do
      let (index', scale') = maybe (0, 0) (bimap ((.&. 0b111) . fromEnum8) fromEnum8) index
          base' = maybe 0 ((.&. 0b111) . fromEnum8) base
      (scale' `shiftL` 6) .|. (index' `shiftL` 3) .|. base'

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
