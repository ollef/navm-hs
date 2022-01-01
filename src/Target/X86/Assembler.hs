{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Target.X86.Assembler where

import Control.Applicative
import Data.Bits
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import Data.Int
import Data.Word
import Target.X86.Assembly

newtype MachineCode = MachineCode Builder
  deriving (Semigroup, Monoid)

assembleInstruction :: Instruction -> MachineCode
assembleInstruction instruction =
  case instruction of
    Add dst src -> do
      let addRI :: Register -> Int64 -> MachineCode
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

word8 :: Word8 -> MachineCode
word8 = MachineCode . Builder.word8

int64 :: Int64 -> MachineCode
int64 = MachineCode . Builder.int64LE

fromEnum8 :: Enum a => a -> Word8
fromEnum8 x =
  fromIntegral (fromEnum x)

instance FromInstruction MachineCode where
  fromInstruction = assembleInstruction

test :: MachineCode
test = getConst $ do
  add rax rax
  add rbx rax
  return ()
