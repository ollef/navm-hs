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
    Add o1 _ o2 -> "  add " <> printOperand o1 <> ", " <> printOperand o2
    Mul (RDX, RAX) RAX o -> "  mul " <> printOperand o
    Mul {} -> error "invalid mul operands"
    Jmp o -> "  jmp " <> printJmpOperand o
    Call o -> "  call " <> printOperand o
    Ret -> "  ret"
    Mov o1 o2 -> "  mov " <> printOperand o1 <> ", " <> printOperand o2
    MovImmediate64 r i -> "  mov " <> printRegister r <> ", " <> Builder.int64Dec i
    Define label -> printLabel label <> ":"

printOperand :: Operand Register -> Builder
printOperand operand =
  case operand of
    Immediate i -> Builder.int32Dec i
    Register r -> printRegister r
    Memory a -> printAddress a

printJmpOperand :: JmpOperand Register -> Builder
printJmpOperand operand =
  case operand of
    JmpRelative Nothing offset -> ".+" <> Builder.int32Dec offset
    JmpRelative (Just label) 0 -> printLabel label
    JmpRelative (Just label) offset -> printLabel label <> "+" <> Builder.int32Dec offset
    JmpAbsolute o -> printOperand o

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
printAddress (Address base maybeLabel disp) =
  case addends of
    [] -> "qword ptr [0]"
    _ -> "qword ptr [" <> mconcat (intersperse "+" addends) <> "]"
  where
    addends =
      catMaybes $
        case base of
          Relative ->
            [Just "rip"]
          Absolute maybeBase maybeIndex ->
            [ printRegister <$> maybeBase
            , printScaledIndex <$> maybeIndex
            ]
          <> [ printLabel <$> maybeLabel
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
