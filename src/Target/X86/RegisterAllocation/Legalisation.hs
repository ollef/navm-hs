{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Target.X86.RegisterAllocation.Legalisation where

import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.BitSet as BitSet
import Register (fresh)
import qualified Register
import Target.X86.Assembly
import qualified Target.X86.Assembly as X86
import qualified Target.X86.Register.Class as X86.Register

legaliseOperands
  :: Instruction Register.Virtual
  -> Register.VirtualSupply [Instruction Register.Virtual]
legaliseOperands instruction = do
  (instruction', (before, after)) <-
    runWriterT $
      X86.Register.constrain
        X86.Register.Constrainers
          { registerOccurrence = \_ _ reg -> pure reg
          , forceSame
          , forceRegister
          }
        instruction
  pure $ before <> [instruction'] <> after
  where
    forceSame dst@(X86.Register.Destination dst') src@(X86.Register.Source src')
      | src' == dst' = pure (dst, src)
    forceSame (X86.Register.Destination dst@(X86.Register _)) (X86.Register.Source src) = do
      tell ([Mov dst src], [])
      pure (X86.Register.Destination dst, X86.Register.Source dst)
    forceSame (X86.Register.Destination dst) (X86.Register.Source src) = do
      reg <- Register <$> lift Register.fresh
      tell ([Mov reg src], [Mov dst reg])
      pure (X86.Register.Destination reg, X86.Register.Source reg)
    forceRegister _ (X86.Register.Source src) = do
      reg <- Register <$> lift Register.fresh
      tell ([Mov reg src], [])
      pure $ X86.Register.Source reg

insertMovesAroundConstrainedOccurrences :: Instruction Register.Virtual -> Register.VirtualSupply [Instruction Register.Virtual]
insertMovesAroundConstrainedOccurrences instruction = do
  (instruction', (before, after)) <- runWriterT $ X86.Register.mapMWithClass go instruction
  pure $ before <> [instruction'] <> after
  where
    go occurrence class_ reg
      | BitSet.size class_ == 1 = do
          reg' <- lift fresh
          tell $ case occurrence of
            X86.Register.Definition -> (mempty, [Mov (Register reg) (Register reg')])
            X86.Register.Use -> ([Mov (Register reg') (Register reg)], mempty)
          pure reg'
      | otherwise = pure reg

concatMapM :: (Applicative m, Monad t, Traversable t) => (a -> m (t b)) -> t a -> m (t b)
concatMapM f = fmap join . traverse f
