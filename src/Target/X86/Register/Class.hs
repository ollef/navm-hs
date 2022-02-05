{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NamedFieldPuns #-}

module Target.X86.Register.Class where

import Data.Bits
import Data.Word
import Target.X86.Assembly
import Prelude hiding (any)

data Class = Class {mask :: !Word8, pattern :: !Word8}

exact :: Register -> Class
exact reg = Class {mask = 0b1111, pattern = fromEnum8 reg}

any :: Class
any = Class {mask = 0, pattern = 0}

contains :: Register -> Class -> Bool
contains reg Class {mask, pattern} = fromEnum8 reg .&. mask == pattern

registers :: Class -> [Register]
registers Class {mask, pattern} = do
  b3 <- b 3
  b2 <- b 2
  b1 <- b 1
  b0 <- b 0
  pure $ toEnum $ fromIntegral $ b3 .|. b2 .|. b1 .|. b0
  where
    b i
      | testBit mask i = [bit i .&. pattern]
      | otherwise = [0, bit i]

fromEnum8 :: Enum a => a -> Word8
fromEnum8 x =
  fromIntegral (fromEnum x)

instance FromRegister Class where
  fromRegister = exact

mapWithClass :: (Class -> reg -> reg') -> Instruction reg -> Instruction reg'
mapWithClass f instruction =
  case instruction of
    Add dst src1 src2 -> Add (f any <$> dst) (f any <$> src1) (f any <$> src2)
    Mul (dst1, dst2) src1 src2 -> Mul (f rdx dst1, f rax dst2) (f rax src1) (f any <$> src2)
    Call o -> Call $ f any <$> o
    Ret -> Ret
    Mov dst src -> Mov (f any <$> dst) (f any <$> src)
    Define label -> Define label

data Constraint reg
  = Same !reg !reg
  | Unsatisifable
  deriving (Eq, Show)

constraints :: Eq reg => Instruction reg -> [Constraint reg]
constraints instruction =
  case instruction of
    Add dst src1 _src2 -> sameOperands dst src1
    Mul {} -> mempty
    Call {} -> mempty
    Ret -> mempty
    Mov {} -> mempty
    Define {} -> mempty

sameRegisters :: Eq reg => reg -> reg -> [Constraint reg]
sameRegisters reg1 reg2
  | reg1 == reg2 = []
  | otherwise = [Same reg1 reg2]

sameOperands :: Eq reg => Operand reg -> Operand reg -> [Constraint reg]
sameOperands (Immediate imm1) (Immediate imm2)
  | imm1 == imm2 = []
  | otherwise = [Unsatisifable]
sameOperands (Immediate _) _ = [Unsatisifable]
sameOperands (Register reg1) (Register reg2) = sameRegisters reg1 reg2
sameOperands (Register _) _ = [Unsatisifable]
sameOperands (Memory addr1) (Memory addr2) = sameAddresses addr1 addr2
sameOperands (Memory _) _ = [Unsatisifable]

sameAddresses :: Eq reg => Address reg -> Address reg -> [Constraint reg]
sameAddresses (Address base1 label1 imm1) (Address base2 label2 imm2)
  | imm1 == imm2 && label1 == label2 =
    sameBases base1 base2
  | otherwise = [Unsatisifable]

sameBases :: Eq reg => Base reg -> Base reg -> [Constraint reg]
sameBases (Absolute base1 index1) (Absolute base2 index2) =
  case (base1, base2) of
    (Nothing, Nothing) -> []
    (Just reg1, Just reg2) -> sameRegisters reg1 reg2
    _ -> [Unsatisifable]
    <> case (index1, index2) of
      (Nothing, Nothing) -> []
      (Just (reg1, scale1), Just (reg2, scale2)) | scale1 == scale2 -> sameRegisters reg1 reg2
      _ -> [Unsatisifable]
sameBases Relative Relative = []
sameBases _ _ = [Unsatisifable]
