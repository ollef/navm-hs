{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

assembleInstruction :: Instruction -> MachineCodeBuilder
assembleInstruction instruction =
  case instruction of
    Add dst src ->
      case (dst, src) of
        (Register dstReg, Register srcReg) -> do
          let dstRegWord = fromEnum8 dstReg
              dstRexReg = dstRegWord `shiftR` 3
              dstRegOp = dstRegWord .&. 7
          let srcRegWord = fromEnum8 srcReg
              srcRexReg = srcRegWord `shiftR` 3
              srcRegOp = srcRegWord .&. 7
          word8 (0x48 .|. dstRexReg .|. (srcRexReg `shiftL` 2)) -- REX prefix
            <> word8 0x01 -- ADD
            <> word8 (0xc0 .|. dstRegOp .|. (srcRegOp `shiftL` 3))
        (Register r, Immediate (toImm8 -> Just imm8)) -> do
          let regWord = fromEnum8 r
              rexReg = regWord `shiftR` 3
              regOp = regWord .&. 7
          word8 (0x48 .|. rexReg) -- REX prefix
            <> word8 0x83 -- ADD
            <> word8 (0xc0 .|. regOp)
            <> word8 imm8
        (Register RAX, Immediate (toImm32 -> Just imm32)) ->
          word8 0x48 -- REX prefix
            <> word8 0x05 -- ADD
            <> int32 imm32
        (Register r, Immediate (toImm32 -> Just imm32)) -> do
          let regWord = fromEnum8 r
              rexReg = regWord `shiftR` 3
              regOp = regWord .&. 7
          word8 (0x48 .|. rexReg) -- REX prefix
            <> word8 0x81 -- ADD
            <> word8 (0xc0 .|. regOp)
            <> int32 imm32
        (Register _, Immediate _) -> error "immediate operand has to fit in 32 bits"
        (Immediate _, _) -> error "immediate destination operand"
        _ -> mempty
    Call (Immediate (toImm32 -> Just imm32)) ->
      word8 0xe8
        <> int32 imm32
    Call (Immediate _) -> error "immediate operand has to fit in 32 bits"
    Call _ -> mempty
    Ret -> word8 0xc3 -- RET
    Mov _ _ -> mempty
  where
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
