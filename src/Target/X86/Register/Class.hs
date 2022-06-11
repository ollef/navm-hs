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
    Jmp o -> Jmp (f any <$> o)
    Call o -> Call $ f any <$> o
    Ret -> Ret
    Mov dst src -> Mov (f any <$> dst) (f any <$> src)
    MovImmediate64 dst src -> MovImmediate64 (f any dst) src
    Define label -> Define label

data Constraint reg
  = Same !reg !reg
  deriving (Eq, Show)

constraints :: Eq reg => Instruction reg -> Maybe [Constraint reg]
constraints instruction =
  case instruction of
    Add dst src1 _src2 -> sameOperands dst src1
    Mul {} -> Just mempty
    Jmp {} -> Just mempty
    Call {} -> Just mempty
    Ret -> Just mempty
    Mov {} -> Just mempty
    MovImmediate64 {} -> Just mempty
    Define {} -> Just mempty

sameRegisters :: Eq reg => reg -> reg -> [Constraint reg]
sameRegisters reg1 reg2
  | reg1 == reg2 = []
  | otherwise = [Same reg1 reg2]

sameOperands :: Eq reg => Operand reg -> Operand reg -> Maybe [Constraint reg]
sameOperands (Immediate imm1) (Immediate imm2)
  | imm1 == imm2 = Just []
  | otherwise = Nothing
sameOperands (Immediate _) _ = Nothing
sameOperands (Register reg1) (Register reg2) = Just $ sameRegisters reg1 reg2
sameOperands (Register _) _ = Nothing
sameOperands (Memory addr1) (Memory addr2) = sameAddresses addr1 addr2
sameOperands (Memory _) _ = Nothing

sameAddresses :: Eq reg => Address reg -> Address reg -> Maybe [Constraint reg]
sameAddresses (Address base1 label1 imm1) (Address base2 label2 imm2)
  | imm1 == imm2 && label1 == label2 =
      sameBases base1 base2
  | otherwise = Nothing

sameBases :: Eq reg => Base reg -> Base reg -> Maybe [Constraint reg]
sameBases (Absolute base1 index1) (Absolute base2 index2) =
  case (base1, base2) of
    (Nothing, Nothing) -> Just []
    (Just reg1, Just reg2) -> Just $ sameRegisters reg1 reg2
    _ -> Nothing
    <> case (index1, index2) of
      (Nothing, Nothing) -> Just []
      (Just (reg1, scale1), Just (reg2, scale2)) | scale1 == scale2 -> Just $ sameRegisters reg1 reg2
      _ -> Nothing
sameBases Relative Relative = Just []
sameBases _ _ = Nothing
