{-# LANGUAGE DisambiguateRecordFields #-}

module Target.X86.RegisterAllocation.Legalisation where

import Control.Monad
import qualified Register
import Target.X86.Assembly

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

concatMapM :: (Applicative m, Monad t, Traversable t) => (a -> m (t b)) -> t a -> m (t b)
concatMapM f = fmap join . traverse f
