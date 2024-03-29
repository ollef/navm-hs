{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}

module Target.X86.RegisterAllocation.SpillInsertion where

import Control.Monad.Writer
import qualified Target.X86.Assembly as X86
import Target.X86.Constraints
import Target.X86.RegisterAllocation (Location (..), StackSlot (StackSlot), scratchRegister)

insertSpills :: X86.Instruction Location -> [X86.Instruction X86.Register]
insertSpills instruction = do
  let (instruction', (before, after)) = runWriter $ mapMWithClass go instruction
  before <> [instruction'] <> after
  where
    go occurrence _ location =
      case location of
        Register reg -> pure reg
        Stack (StackSlot slot) -> case occurrence of
          Definition -> do
            tell ([], [X86.mov [X86.rsp + fromIntegral (slot * 8)] scratchRegister])
            pure scratchRegister
          Use -> do
            tell ([X86.mov scratchRegister [X86.rsp + fromIntegral (slot * 8)]], [])
            pure scratchRegister
