{-# LANGUAGE OverloadedStrings #-}

module Target.X86.Printer where

import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import Data.List (intersperse)
import Data.Maybe
import Target.X86.Assembly

printInstructions :: [Instruction] -> Builder
printInstructions =
  mconcat . map (\i -> printInstruction i <> "\n")

printInstruction :: Instruction -> Builder
printInstruction instruction =
  case instruction of
    Add o1 o2 -> "add\t" <> printOperand o1 <> ", " <> printOperand o2
    Call o -> "call\t" <> printOperand o
    Ret -> "ret"
    Mov o1 o2 -> "mov\t" <> printOperand o1 <> ", " <> printOperand o2

printOperand :: Operand -> Builder
printOperand operand =
  case operand of
    Immediate i -> Builder.int64Dec i
    Register r -> printRegister r
    Address a -> printAddress a

printRegister :: Register -> Builder
printRegister register =
  case register of
    RAX -> "rax"
    RCX -> "rcx"
    RDX -> "rdx"
    RBX -> "rbx"
    RSP -> "rsp"
    RBP -> "rbp"
    RSI -> "rsi"
    RDI -> "rdi"
    R8 -> "r8"
    R9 -> "r9"
    R10 -> "r10"
    R11 -> "r11"
    R12 -> "r12"
    R13 -> "r13"
    R14 -> "r14"
    R15 -> "r15"

printAddress :: Address -> Builder
printAddress (Address' maybeBase maybeIndex disp) =
  case addends of
    [] -> "[0]"
    _ -> "[" <> mconcat (intersperse "+" addends) <> "]"
  where
    addends =
      catMaybes
        [ printRegister <$> maybeBase
        , printScaledIndex <$> maybeIndex
        , printDisplacement disp
        ]
    printScaledIndex (index, scale) =
      printRegister index
        <> case scale of
          Scale1 -> mempty
          Scale2 -> "*2"
          Scale4 -> "*4"
          Scale8 -> "*8"
    printDisplacement 0 = Nothing
    printDisplacement d = Just $ Builder.int32Dec d
