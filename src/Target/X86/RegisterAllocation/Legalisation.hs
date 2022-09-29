{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module Target.X86.RegisterAllocation.Legalisation where

import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EnumMap
import Data.Functor.Compose
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Register (fresh)
import qualified Register
import Target.X86.Assembly
import qualified Target.X86.Register.Class as X86.Register

legaliseOperands
  :: Instruction Register.Virtual
  -> Register.VirtualSupply [Instruction Register.Virtual]
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
      | a /= b ->
          concatMapM
            legaliseOperands
            [ Mov a b
            , Add a a c
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
        , Mov a b'
        ]
    Mov {} -> pure [instruction]
    MovImmediate64 {} -> pure [instruction]
    Define {} -> pure [instruction]

type RegisterVariants =
  EnumMap Register.Virtual (HashMap X86.Register.Class Register.Virtual)

splitRegistersWithDifferingOccurrenceClasses
  :: Instruction Register.Virtual
  -> StateT RegisterVariants Register.VirtualSupply [Instruction Register.Virtual]
splitRegistersWithDifferingOccurrenceClasses instruction = do
  (instruction', (before, after)) <- runWriterT $ X86.Register.mapMWithClass go instruction
  pure $ before <> [instruction'] <> after
  where
    go occurrence class_ reg = do
      let alterReg Nothing =
            Compose $
              pure (reg, Just $ HashMap.singleton class_ reg)
          alterReg (Just classMap) = Compose $ do
            (reg', classMap') <- getCompose $ HashMap.alterF alterClass class_ classMap
            pure
              ( reg'
              , Just classMap'
              )
          alterClass Nothing = Compose $ do
            reg' <- lift $ lift fresh
            tell $ case occurrence of
              X86.Register.Definition ->
                (mempty, [Mov (Register reg) (Register reg')])
              X86.Register.Use ->
                ([Mov (Register reg') (Register reg)], mempty)
            pure (reg', Just reg')
          alterClass (Just reg') =
            Compose $
              pure (reg', Just reg')
      variants <- get
      (reg', variants') <- getCompose $ EnumMap.alterF alterReg reg variants
      put variants'
      pure reg'

concatMapM :: (Applicative m, Monad t, Traversable t) => (a -> m (t b)) -> t a -> m (t b)
concatMapM f = fmap join . traverse f
