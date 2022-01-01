{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
    Add dst src -> do
      let addRI :: Register -> Int64 -> MachineCodeBuilder
          addRI RAX i =
            word8 0x48 -- REX prefix
              <> word8 0x05 -- ADD
              <> int64 i
          addRI r i = do
            undefined

      case (dst, src) of
        (Register dstReg, Register srcReg) ->
          word8 0x48 -- REX prefix
            <> word8 0x01 -- ADD
            <> word8 (0xc0 .|. fromEnum8 srcReg .|. (fromEnum8 dstReg `shiftL` 3))
        (Register r, Immediate i) -> addRI r i
        (Immediate _, _) -> mempty
        _ -> undefined
    Call _ -> undefined
    Ret -> word8 0xc3 -- RET
    Mov _ _ -> undefined

word8 :: Word8 -> MachineCodeBuilder
word8 = MachineCodeBuilder . Builder.word8

int64 :: Int64 -> MachineCodeBuilder
int64 = MachineCodeBuilder . Builder.int64LE

fromEnum8 :: Enum a => a -> Word8
fromEnum8 x =
  fromIntegral (fromEnum x)

instance FromInstruction MachineCodeBuilder where
  fromInstruction = assembleInstruction

test :: MachineCode
test = getConst $ do
  add rax rax
  add rbx rax
  return ()
