{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NamedFieldPuns #-}

module Target.X86.Register where

import Data.Bits
import Data.Word

data Register = RAX | RCX | RDX | RBX | RSP | RBP | RSI | RDI | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  deriving (Show, Eq, Ord, Enum, Bounded)

rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15 :: FromRegister a => a
rax = fromRegister RAX
rcx = fromRegister RCX
rdx = fromRegister RDX
rbx = fromRegister RBX
rsp = fromRegister RSP
rbp = fromRegister RBP
rsi = fromRegister RSI
rdi = fromRegister RDI
r8 = fromRegister R8
r9 = fromRegister R9
r10 = fromRegister R10
r11 = fromRegister R11
r12 = fromRegister R12
r13 = fromRegister R13
r14 = fromRegister R14
r15 = fromRegister R15

data Class = Class {mask :: !Word8, pattern :: !Word8}

exact :: Register -> Class
exact reg = Class {mask = 0b1111, pattern = fromEnum8 reg}

contains :: Register -> Class -> Bool
contains reg Class {mask, pattern} = fromEnum8 reg .&. mask == pattern

registers :: Class -> [Register]
registers Class {mask, pattern} = do
  b0 <- b 0
  b1 <- b 1
  b2 <- b 2
  b3 <- b 3
  pure $ toEnum $ fromIntegral $ b3 .|. b2 .|. b1 .|. b0
  where
    b i
      | testBit mask i = [bit i .&. pattern]
      | otherwise = [0, bit i]

fromEnum8 :: Enum a => a -> Word8
fromEnum8 x =
  fromIntegral (fromEnum x)

class FromRegister a where
  fromRegister :: Register -> a

instance FromRegister Register where
  fromRegister = id

instance FromRegister Class where
  fromRegister = exact
