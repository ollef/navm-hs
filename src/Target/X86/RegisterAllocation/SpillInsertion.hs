{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}

module Target.X86.RegisterAllocation.SpillInsertion where

import Control.Monad.Writer
import qualified Target.X86.Assembly as X86
import qualified Target.X86.Register.Class as X86.Register
import Target.X86.RegisterAllocation (Location (..), StackSlot (StackSlot), scratchRegister)

insertSpills :: X86.Instruction Location -> [X86.Instruction X86.Register]
insertSpills instruction = do
  let (instruction', (before, after)) = runWriter $ X86.Register.mapMWithClass go instruction
  before <> [instruction'] <> after
  where
    go occurrence _ location =
      case location of
        Register reg -> pure reg
        Stack (StackSlot slot) -> case occurrence of
          X86.Register.Definition -> do
            tell ([], [X86.Mov [X86.rsp + fromIntegral slot * 8] (X86.Register scratchRegister)])
            pure scratchRegister
          X86.Register.Use -> do
            tell ([X86.Mov (X86.Register scratchRegister) [X86.rsp + fromIntegral slot * 8]], [])
            pure scratchRegister
