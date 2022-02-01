{-# LANGUAGE OverloadedStrings #-}

module Target.X86.Printer where

import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import Data.List (intersperse)
import Data.Maybe
import Label (Label)
import qualified Label
import Target.X86.Assembly

printInstructions :: [Instruction Register] -> Builder
printInstructions =
  mconcat . map (\i -> printInstruction i <> "\n")

printInstruction :: Instruction Register -> Builder
printInstruction instruction =
  case instruction of
    Add o1 _ o2 -> "add\t" <> printOperand o1 <> ", " <> printOperand o2
    Mul (RDX, RAX) RAX o -> "mul\t" <> printOperand o
    Mul {} -> error "invalid mul operands"
    Call o -> "call\t" <> printOperand o
    Ret -> "ret"
    Mov o1 o2 -> "mov\t" <> printOperand o1 <> ", " <> printOperand o2

printOperand :: Operand Register -> Builder
printOperand operand =
  case operand of
    Immediate (Constant i) -> Builder.int64Dec i
    Immediate (Label l) -> printLabel l
    Register r -> printRegister r
    Memory a -> printAddress a

printLabel :: Label -> Builder
printLabel (Label.Label l) = ".L" <> Builder.byteString l

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

printAddress :: Address Register -> Builder
printAddress (Address maybeBase maybeIndex disp) =
  case addends of
    [] -> "qword ptr [0]"
    _ -> "qword ptr [" <> mconcat (intersperse "+" addends) <> "]"
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
    printDisplacement (Constant 0) = Nothing
    printDisplacement (Constant d) = Just $ Builder.int32Dec d
    printDisplacement (Label l) = Just $ printLabel l
