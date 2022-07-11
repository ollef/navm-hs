{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Target.X86.Register.Class where

import Data.BitSet (BitSet)
import qualified Data.BitSet as BitSet
import Data.Hashable
import Data.Word
import GHC.Exts (IsList (..))
import Register (FromRegister (..), RegisterType)
import Target.X86.Assembly
import Prelude hiding (any)

newtype Class = Class {bitSet :: BitSet Register}
  deriving (Eq, Show, Hashable)

instance IsList Class where
  type Item Class = Register
  toList (Class c) = BitSet.toList c
  fromList = Class . BitSet.fromList

exact :: Register -> Class
exact = Class . BitSet.singleton

any :: Class
any = Class BitSet.full

contains :: Register -> Class -> Bool
contains reg (Class c) = BitSet.member reg c

fromEnum8 :: Enum a => a -> Word8
fromEnum8 x =
  fromIntegral (fromEnum x)

type instance RegisterType Class = Register

instance FromRegister Class where
  fromRegister = exact

data Occurrence = Definition | Use
  deriving (Eq, Show)

mapWithClass ::
  Monad m =>
  (Occurrence -> Class -> reg -> m reg') ->
  Instruction reg ->
  m (Instruction reg')
mapWithClass f instruction =
  case instruction of
    Add dst src1 src2 -> Add <$> def any dst <*> use any src1 <*> use any src2
    Mul (dst1, dst2) src1 src2 ->
      Mul <$> ((,) <$> f Definition rdx dst1 <*> f Definition rax dst2) <*> f Use rax src1 <*> use any src2
    Jmp o -> Jmp <$> mapM (f Use any) o
    Call o -> Call <$> mapM (f Use any) o
    Ret -> pure Ret
    Mov dst src -> Mov <$> def any dst <*> use any src
    MovImmediate64 dst src -> MovImmediate64 <$> f Definition any dst <*> pure src
    Define label -> pure $ Define label
  where
    use class_ operand = mapM (f Use class_) operand
    def class_ (Register r) = Register <$> f Definition class_ r
    def class_ operand = use class_ operand
