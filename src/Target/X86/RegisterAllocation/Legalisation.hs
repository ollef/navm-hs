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
import Target.X86.Constraints

legaliseOperands
  :: Instruction Register.Virtual
  -> Register.VirtualSupply [Instruction Register.Virtual]
legaliseOperands instruction = do
  (instruction', (before, after)) <-
    runWriterT $
      constrain
        Constrainers
          { registerOccurrence = \_ _ reg -> pure reg
          , forceSame
          , forceRegister
          }
        instruction
  pure $ before <> [instruction'] <> after
  where
    forceSame dst@(Destination dst') src@(Source src')
      | src' == dst' = pure (dst, src)
    forceSame (Destination dst@(X86.Register _)) (Source src) = do
      tell ([Mov dst src], [])
      pure (Destination dst, Source dst)
    forceSame (Destination dst) (Source src) = do
      reg <- Register <$> lift Register.fresh
      tell ([Mov reg src], [Mov dst reg])
      pure (Destination reg, Source reg)
    forceRegister _ (Source src) = do
      reg <- Register <$> lift Register.fresh
      tell ([Mov reg src], [])
      pure $ Source reg

insertMovesAroundConstrainedOccurrences :: Instruction Register.Virtual -> Register.VirtualSupply [Instruction Register.Virtual]
insertMovesAroundConstrainedOccurrences instruction = do
  (instruction', (before, after)) <- runWriterT $ mapMWithClass go instruction
  pure $ before <> [instruction'] <> after
  where
    go occurrence class_ reg
      | BitSet.size class_ == 1 = do
          reg' <- lift fresh
          tell $ case occurrence of
            Definition -> (mempty, [Mov (Register reg) (Register reg')])
            Use -> ([Mov (Register reg') (Register reg)], mempty)
          pure reg'
      | otherwise = pure reg

concatMapM :: (Applicative m, Monad t, Traversable t) => (a -> m (t b)) -> t a -> m (t b)
concatMapM f = fmap join . traverse f
