{-# LANGUAGE DisambiguateRecordFields #-}

module Target.X86.RegisterAllocation.Legalisation where

import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Register (fresh)
import qualified Register
import Target.X86.Assembly
import qualified Target.X86.Register.Class as X86.Register

legaliseOperands ::
  Instruction Register.Virtual ->
  Register.VirtualSupply [Instruction Register.Virtual]
legaliseOperands instruction =
  case instruction of
    Add a@(Memory _) b c@(Memory _) -> do
      c' <- Register <$> Register.fresh
      concatMapM
        legaliseOperands
        [ Mov c' c
        , Add a b c'
        ]
    Add a b c
      | a /= b -> do
          c' <- Register <$> Register.fresh
          concatMapM
            legaliseOperands
            [ Mov c' c
            , Add c' c' b
            , Add a a c'
            ]
    Add {} -> pure [instruction]
    Mul a b c@(Immediate _) -> do
      c' <- Register <$> Register.fresh
      concatMapM
        legaliseOperands
        [ Mov c' c
        , Mul a b c'
        ]
    Mul {} -> pure [instruction]
    Jmp {} -> pure [instruction]
    Call {} -> pure [instruction]
    Ret -> pure [Ret]
    Mov a@(Memory _) b@(Memory _) -> do
      b' <- Register <$> Register.fresh
      pure
        [ Mov b' b
        , Mov a b
        ]
    Mov {} -> pure [instruction]
    MovImmediate64 {} -> pure [instruction]
    Define {} -> pure [instruction]

type RegisterVariants =
  HashMap (Register.Virtual, X86.Register.Class) Register.Virtual

splitRegistersWithDifferingOccurrenceClasses ::
  Instruction Register.Virtual ->
  StateT RegisterVariants Register.VirtualSupply [Instruction Register.Virtual]
splitRegistersWithDifferingOccurrenceClasses instruction = do
  (instruction', copies) <- runWriterT $ X86.Register.mapWithClass go instruction
  pure $ copies <> [instruction']
  where
    go _occurrence class_ reg = do
      variants <- get
      let (mreg', variants') = HashMap.alterF alter (reg, class_) variants
          alter Nothing =
            ( do
                reg' <- lift $ lift fresh
                tell [Mov (Register reg') (Register reg)]
                pure reg'
            , Just reg
            )
          alter (Just reg') = (pure reg', Just reg')
      put variants'
      mreg'

concatMapM :: (Applicative m, Monad t, Traversable t) => (a -> m (t b)) -> t a -> m (t b)
concatMapM f = fmap join . traverse f
