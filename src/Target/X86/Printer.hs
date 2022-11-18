{-# LANGUAGE OverloadedStrings #-}

module Target.X86.Printer where

import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import Target.X86.Assembly
import qualified Target.X86.Printer.SSA as SSA

printInstructions :: [Instruction Register] -> Builder
printInstructions =
  mconcat . map (\i -> printInstruction i <> "\n")

printInstruction :: Instruction Register -> Builder
printInstruction instruction =
  case instruction of
    Define _ -> ""
    _ -> "  "
    <> case instruction of
      Add o1 _ o2 -> "add " <> printOperand o1 <> ", " <> printOperand o2
      Mul (RDX, RAX) RAX o -> "mul " <> printOperand o
      Mul {} -> error "invalid mul operands"
      Jmp o -> "jmp " <> printJmpOperand o
      Call o -> "call " <> printOperand o
      Ret -> "ret"
      Int w -> "int 0x" <> Builder.word8Hex w
      Mov o1 o2 -> "mov " <> printOperand o1 <> ", " <> printOperand o2
      MovImmediate64 r i -> "mov " <> printRegister r <> ", " <> Builder.int64Dec i
      Define label -> SSA.printLabel label <> ":"

printOperand :: Operand Register -> Builder
printOperand = SSA.printOperand printRegister

printJmpOperand :: JmpOperand Register -> Builder
printJmpOperand = SSA.printJmpOperand printRegister

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
