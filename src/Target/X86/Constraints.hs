{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

module Target.X86.Constraints where

import qualified Data.BitSet as BitSet
import Data.Bitraversable
import Data.Foldable hiding (any)
import Data.Function
import Data.Functor.Identity
import Target.X86.Assembly
import Prelude hiding (any)

data Occurrence = Definition | Use
  deriving (Eq, Show)

mapWithClass
  :: (Occurrence -> Class -> reg -> reg')
  -> Instruction reg
  -> Instruction reg'
mapWithClass f =
  runIdentity
    . constrain
      Constrainers
        { registerOccurrence = \occ class_ reg -> pure $ f occ class_ reg
        , forceSame = curry pure
        , forceRegister = const pure
        }

mapMWithClass
  :: Monad m
  => (Occurrence -> Class -> reg -> m reg')
  -> Instruction reg
  -> m (Instruction reg')
mapMWithClass f =
  constrain
    Constrainers
      { registerOccurrence = f
      , forceSame = curry pure
      , forceRegister = const pure
      }

foldWithClass :: (Occurrence -> Class -> reg -> a -> a) -> a -> Instruction reg -> a
foldWithClass f def = foldl' (&) def . mapWithClass f

newtype Destination operand = Destination operand

newtype Source operand = Source operand

data Constrainers reg reg' m = Constrainers
  { registerOccurrence :: !(Occurrence -> Class -> reg -> m reg')
  , forceSame :: !(Destination (Operand reg') -> Source (Operand reg') -> m (Destination (Operand reg'), Source (Operand reg')))
  , forceRegister :: !(Class -> Source (Operand reg') -> m (Source (Operand reg')))
  }

constrain
  :: Monad m
  => Constrainers reg reg' m
  -> Instruction reg
  -> m (Instruction reg')
constrain Constrainers {..} instruction =
  case instruction of
    Add dst src1 src2 -> do
      dst' <- def any dst
      src1' <- use any src1
      src2' <- use any src2
      (Destination dst'', Source src1'') <- forceSame (Destination dst') (Source src1')
      Source src2'' <- case (dst'', src2') of
        (Memory _, Memory _) -> forceRegister any $ Source src2'
        _ -> pure $ Source src2'
      pure $ Add dst'' src1'' src2''
    Mul (dst1, dst2) src1 src2 ->
      Mul <$> ((,) <$> registerOccurrence Definition rdx dst1 <*> registerOccurrence Definition rax dst2) <*> registerOccurrence Use rax src1 <*> use any src2
    Jmp (JmpRelative label offset) -> pure $ Jmp $ JmpRelative label offset
    Jmp (JmpAbsolute o) -> Jmp . JmpAbsolute <$> use any o
    Call o -> Call <$> use any o
    Ret -> pure Ret
    Int w -> pure $ Int w
    Mov dst src -> do
      dst' <- def any dst
      src' <- use any src
      Source src'' <- case (dst', src') of
        (Memory _, Memory _) -> forceRegister any $ Source src'
        _ -> pure $ Source src'
      pure $ Mov dst' src''
    MovImmediate64 dst src -> MovImmediate64 <$> registerOccurrence Definition any dst <*> pure src
    Define label -> pure $ Define label
  where
    use class_ (Register r) = Register <$> registerOccurrence Use class_ r
    use _ (Immediate imm) = pure $ Immediate imm
    use class_ (Memory addr) = Memory <$> useAddress class_ addr
    useAddress class_ (Address base maybeLabel offset) = Address <$> useBase class_ base <*> pure maybeLabel <*> pure offset
    useBase class_ (Absolute base index) = Absolute <$> mapM (registerOccurrence Use class_) base <*> mapM (bitraverse (registerOccurrence Use $ BitSet.delete RSP class_) pure) index
    useBase _ Relative = pure Relative
    def class_ (Register r) = Register <$> registerOccurrence Definition class_ r
    def class_ operand = use class_ operand
