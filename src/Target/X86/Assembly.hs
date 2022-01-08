{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Target.X86.Assembly where

import Control.Applicative
import Data.Bifunctor
import Data.Int
import qualified Data.Map as Map
import Data.Maybe
import GHC.Exts

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

data Address = Address' !(Maybe Register) !(Maybe (Register, Scale)) !Int32
  deriving (Show, Eq)

data Scale = Scale1 | Scale2 | Scale4 | Scale8
  deriving (Show, Eq, Enum, Bounded)

toScale :: Integral a => a -> Maybe Scale
toScale a = case fromIntegral a :: Integer of
  1 -> Just Scale1
  2 -> Just Scale2
  4 -> Just Scale4
  8 -> Just Scale8
  _ -> Nothing

fromScale :: Num a => Scale -> a
fromScale s = case s of
  Scale1 -> 1
  Scale2 -> 2
  Scale4 -> 4
  Scale8 -> 8

scaledRegister :: Integral a => Register -> a -> Maybe (Maybe Register, Maybe (Register, Scale))
scaledRegister _ 0 = Just (Nothing, Nothing)
scaledRegister reg n
  | Just Scale1 <- toScale n = Just (Just reg, Nothing)
  | Just scale <- toScale n = Just (Nothing, Just (reg, scale))
  | Just scale <- toScale $ n - 1 = Just (Just reg, Just (reg, scale))
  | otherwise = Nothing

data Operand
  = Immediate !Int64
  | Register !Register
  | Address !Address
  deriving (Show, Eq)

data Instruction
  = Add Operand Operand
  | Mul !(Register, Register) Register Operand
  | Call Operand
  | Ret
  | Mov Operand Operand
  deriving (Show, Eq)

add :: FromInstruction i => Operand -> Operand -> i
add o1 o2 = fromInstruction $ Add o1 o2

ret :: FromInstruction i => i
ret = fromInstruction Ret

call :: FromInstruction i => Operand -> i
call = fromInstruction . Call

mov :: FromInstruction i => Operand -> Operand -> i
mov o1 o2 = fromInstruction $ Mov o1 o2

mul :: FromInstruction i => (Register, Register) -> Register -> Operand -> i
mul out o1 o2 = fromInstruction $ Mul out o1 o2

instance Num Operand where
  fromInteger = Immediate . fromInteger
  _ + _ = error "not implemented"
  _ - _ = error "not implemented"
  _ * _ = error "not implemented"
  abs = error "not implemented"
  signum = error "not implemented"

instance Num Address where
  fromInteger i = Address' Nothing Nothing (fromInteger i)
  Address' base1 index1 disp1 + Address' base2 index2 disp2 =
    Address' base index (disp1 + disp2)
    where
      regScales =
        Map.fromListWith (+) $
          catMaybes
            [ (,1 :: Int32) <$> base1
            , second fromScale <$> index1
            , (,1) <$> base2
            , second fromScale <$> index2
            ]
      (base, index) =
        case Map.toList regScales of
          [] -> (Nothing, Nothing)
          [(reg, scale)]
            | Just result <- scaledRegister reg scale -> result
          [_] -> error "unencodable scale"
          [(reg1, 1), (reg2, toScale -> Just scale2)] -> (Just reg1, Just (reg2, scale2))
          [(reg1, toScale -> Just scale1), (reg2, 1)] -> (Just reg2, Just (reg1, scale1))
          [_, _] -> error "can only scale one register in address operand"
          _ : _ : _ : _ -> error "too many registers in address operand"
  negate (Address' Nothing Nothing d) = Address' Nothing Nothing (negate d)
  negate _ = error "can't negate address operand based on register(s)"
  Address' Nothing Nothing 1 * a = a
  Address' Nothing Nothing 0 * _ = 0
  a * Address' Nothing Nothing 1 = a
  _ * Address' Nothing Nothing 0 = 0
  Address' base1 index1 0 * Address' Nothing Nothing disp =
    Address' base index 0
    where
      regScales =
        Map.fromListWith (+) $
          catMaybes
            [ (,1 :: Int32) <$> base1
            , second fromScale <$> index1
            ]
      (base, index) = case Map.toList regScales of
        [] -> (Nothing, Nothing)
        [(reg, scale)]
          | Just result <- scaledRegister reg (scale * disp) -> result
        _ -> error "can't multiply address operands"
  Address' Nothing Nothing disp * Address' base2 index2 0 =
    Address' base index 0
    where
      regScales =
        Map.fromListWith (+) $
          catMaybes
            [ (,1 :: Int32) <$> base2
            , second fromScale <$> index2
            ]
      (base, index) = case Map.toList regScales of
        [] -> (Nothing, Nothing)
        [(reg, scale)]
          | Just result <- scaledRegister reg (scale * disp) -> result
        _ -> error "can't multiply address operands"
  _ * _ = error "can't multiply address operands"
  abs = error "abs"
  signum = error "signum"

class FromRegister a where
  fromRegister :: Register -> a

instance FromRegister Register where
  fromRegister = id

instance FromRegister Address where
  fromRegister r = Address' (Just r) Nothing 0

instance FromRegister Operand where
  fromRegister = Register

class FromAddress a where
  fromAddress :: Address -> a

instance FromAddress Address where
  fromAddress = id

instance FromAddress Operand where
  fromAddress = Address

instance IsList Operand where
  type Item Operand = Address
  fromList [addr] = Address addr
  fromList _ = error "address operand list doesn't have one element"
  toList (Address addr) = [addr]
  toList _ = error "operand isn't an address"

class FromInstruction a where
  fromInstruction :: Instruction -> a

instance FromInstruction Instruction where
  fromInstruction = id

instance FromInstruction a => FromInstruction (Const a b) where
  fromInstruction = Const . fromInstruction

instance FromInstruction a => FromInstruction [a] where
  fromInstruction = pure . fromInstruction
