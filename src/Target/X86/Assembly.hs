{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Target.X86.Assembly (
  module Target.X86.Assembly
  , module X
) where

import Control.Applicative
import Data.Bifunctor
import Data.Int
import Data.Kind
import qualified Data.Map as Map
import Data.Maybe
import GHC.Exts
import Label (Label)
import Target.X86.Register as X

data Instruction reg
  = Add (Operand reg) (Operand reg) (Operand reg)
  | Mul !(reg, reg) reg (Operand reg)
  | Jmp (JmpOperand reg)
  | Call (Operand reg)
  | Ret
  | Mov (Operand reg) (Operand reg)
  | MovImmediate64 !reg !Int64
  | Define !Label
  deriving (Show, Eq, Functor, Foldable, Traversable)

data Operand reg
  = Immediate !Int32
  | Register !reg
  | Memory !(Address reg)
  deriving (Show, Eq, Functor, Foldable, Traversable)

data JmpOperand reg
  = JmpRelative !(Maybe Label) !Int32
  | JmpAbsolute !(Operand reg)
  deriving (Show, Eq, Functor, Foldable, Traversable)

data Address reg
  = Address !(Base reg) !(Maybe Label) !Int32
  deriving (Show, Eq, Functor, Foldable, Traversable)

data Base reg
  = Absolute !(Maybe reg) !(Maybe (reg, Scale))
  | Relative
  deriving (Show, Eq, Functor, Foldable, Traversable)

data Scale = Scale1 | Scale2 | Scale4 | Scale8
  deriving (Show, Eq, Enum, Bounded)

add :: (reg ~ RegisterType i, FromInstruction i) => Operand reg -> Operand reg -> Operand reg -> i
add o1 o2 o3 = fromInstruction $ Add o1 o2 o3

mul :: (reg ~ RegisterType i, FromInstruction i) => (reg, reg) -> reg -> Operand reg -> i
mul out o1 o2 = fromInstruction $ Mul out o1 o2

jmp :: (reg ~ RegisterType i, FromInstruction i) => JmpOperand reg -> i
jmp = fromInstruction . Jmp

call :: (reg ~ RegisterType i, FromInstruction i) => Operand reg -> i
call = fromInstruction . Call

ret :: FromInstruction i => i
ret = fromInstruction Ret

mov :: (reg ~ RegisterType i, FromInstruction i) => Operand reg -> Operand reg -> i
mov o1 o2 = fromInstruction $ Mov o1 o2

movi64 :: (reg ~ RegisterType i, FromInstruction i) => reg -> Int64 -> i
movi64 r i = fromInstruction $ MovImmediate64 r i

define :: FromInstruction i => Label -> i
define = fromInstruction . Define

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

scaledRegister :: Integral a => reg -> a -> Maybe (Maybe reg, Maybe (reg, Scale))
scaledRegister _ 0 = Just (Nothing, Nothing)
scaledRegister reg n
  | Just Scale1 <- toScale n = Just (Just reg, Nothing)
  | Just scale <- toScale n = Just (Nothing, Just (reg, scale))
  | Just scale <- toScale $ n - 1 = Just (Just reg, Just (reg, scale))
  | otherwise = Nothing

instance Num (Operand reg) where
  fromInteger = Immediate . fromInteger
  _ + _ = error "not implemented"
  _ - _ = error "not implemented"
  _ * _ = error "not implemented"
  abs = error "not implemented"
  signum = error "not implemented"

instance Ord reg => Semigroup (Base reg) where
  Absolute Nothing Nothing <> Relative = Relative
  Relative <> Absolute Nothing Nothing = Relative
  Absolute base1 index1 <> Absolute base2 index2 = Absolute base index
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
  _ <> _ = error "Can't add base addresses"

instance Ord reg => Monoid (Base reg) where
  mempty = Absolute Nothing Nothing

instance Ord reg => Num (Address reg) where
  fromInteger i = Address mempty Nothing $ fromInteger i
  Address base1 label1 disp1 + Address base2 label2 disp2 =
    Address (base1 <> base2) label disp
    where
      label = case (label1, label2) of
        (Nothing, l) -> l
        (l, Nothing) -> l
        (Just _, Just _) -> error "too many labels in address operand"
      disp = disp1 + disp2
  negate (Address (Absolute Nothing Nothing) Nothing d) = Address (Absolute Nothing Nothing) Nothing $ negate d
  negate _ = error "can't negate address operand based on register(s)"
  Address (Absolute Nothing Nothing) Nothing 1 * a = a
  Address (Absolute Nothing Nothing) Nothing 0 * _ = 0
  a * Address (Absolute Nothing Nothing) Nothing 1 = a
  _ * Address (Absolute Nothing Nothing) Nothing 0 = 0
  Address (Absolute base1 index1) Nothing 0 * Address (Absolute Nothing Nothing) Nothing disp = Address (Absolute base index) Nothing 0
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
  Address (Absolute Nothing Nothing) Nothing disp * Address (Absolute base2 index2) Nothing 0 =
    Address (Absolute base index) Nothing 0
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

instance Num (JmpOperand reg) where
  fromInteger = JmpRelative Nothing . fromInteger
  JmpRelative label1 offset1 + JmpRelative label2 offset2 = JmpRelative label offset
    where
      label = case (label1, label2) of
        (Nothing, l2) -> l2
        (l1, Nothing) -> l1
        (Just _, Just _) -> error "can't add labels"
      offset = offset1 + offset2
  _ + _ = error "can only add relative jmp operands"
  JmpRelative Nothing offset1 * JmpRelative Nothing offset2 = JmpRelative Nothing $ offset1 * offset2
  JmpRelative Nothing 0 * _ = 0
  _ * JmpRelative Nothing 0 = 0
  JmpRelative Nothing 1 * o = o
  o * JmpRelative Nothing 1 = o
  _ * _ = error "can't multiply jmp operands"
  negate (JmpRelative Nothing offset) = JmpRelative Nothing $ negate offset
  negate _ = error "can't negate jmp operand"
  abs = error "abs"
  signum = error "signum"

type family RegisterType a :: Type

type instance RegisterType (Address reg) = reg

type instance RegisterType (Operand reg) = reg

type instance RegisterType (JmpOperand reg) = reg

type instance RegisterType (Instruction reg) = reg

type instance RegisterType [a] = RegisterType a

type instance RegisterType (Const a b) = RegisterType a

instance reg ~ Register => FromRegister (Address reg) where
  fromRegister r = Address (Absolute (Just r) Nothing) Nothing 0

instance reg ~ Register => FromRegister (Operand reg) where
  fromRegister = Register

instance reg ~ Register => FromRegister (JmpOperand reg) where
  fromRegister = JmpAbsolute . fromRegister

class FromAddress a where
  fromAddress :: Address (RegisterType a) -> a

instance FromAddress (Address reg) where
  fromAddress = id

instance FromAddress (Operand reg) where
  fromAddress = Memory

instance FromAddress (JmpOperand reg) where
  fromAddress = JmpAbsolute . fromAddress

rip :: FromAddress a => a
rip = fromAddress $ Address Relative Nothing 0

instance IsList (Operand reg) where
  type Item (Operand reg) = Address reg
  fromList [addr] = Memory addr
  fromList _ = error "address operand list doesn't have one element"
  toList (Memory addr) = [addr]
  toList _ = error "operand isn't an address"

instance IsList (JmpOperand reg) where
  type Item (JmpOperand reg) = Address reg
  fromList = JmpAbsolute . fromList
  toList (JmpAbsolute (Memory addr)) = [addr]
  toList _ = error "operand isn't an address"

instance Ord reg => IsString (Address reg) where
  fromString s = Address mempty (Just $ fromString s) 0

instance IsString (JmpOperand reg) where
  fromString s = JmpRelative (Just $ fromString s) 0

class FromInstruction a where
  fromInstruction :: Instruction (RegisterType a) -> a

instance FromInstruction (Instruction reg) where
  fromInstruction = id

instance FromInstruction a => FromInstruction (Const a b) where
  fromInstruction = Const . fromInstruction

instance FromInstruction a => FromInstruction [a] where
  fromInstruction = pure . fromInstruction
