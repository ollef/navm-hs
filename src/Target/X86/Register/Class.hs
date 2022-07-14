{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Target.X86.Register.Class where

import Data.BitSet (BitSet)
import qualified Data.BitSet as BitSet
import Target.X86.Assembly
import Prelude hiding (any)

type Class = BitSet Register

data Occurrence = Definition | Use
  deriving (Eq, Show)

any :: Class
any = BitSet.full
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
