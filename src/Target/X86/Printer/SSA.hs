{-# LANGUAGE OverloadedStrings #-}

module Target.X86.Printer.SSA where

import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import Data.List (intersperse)
import Data.Maybe
import Label (Label)
import qualified Label
import Target.X86.Assembly

printInstructions :: (v -> Builder) -> [Instruction v] -> Builder
printInstructions printRegister =
  mconcat . map (\i -> printInstruction printRegister i <> "\n")

printInstruction :: (v -> Builder) -> Instruction v -> Builder
printInstruction printRegister instruction =
  case instruction of
    Add a b c ->
      "  "
        <> printOperand printRegister a
        <> " = add "
        <> printOperand printRegister b
        <> ", "
        <> printOperand printRegister c
    Mul (a, b) c d ->
      "  ("
        <> printRegister a
        <> ", "
        <> printRegister b
        <> ") = mul "
        <> printRegister c
        <> ", "
        <> printOperand printRegister d
    Jmp o -> "  jmp " <> printJmpOperand printRegister o
    Call o -> "  call " <> printOperand printRegister o
    Ret -> "  ret"
    Mov a b -> "  " <> printOperand printRegister a <> " = mov " <> printOperand printRegister b
    MovImmediate64 r i -> "  " <> printRegister r <> " = mov " <> Builder.int64Dec i
    Define label -> printLabel label <> ":"

printOperand :: (v -> Builder) -> Operand v -> Builder
printOperand printRegister operand =
  case operand of
    Immediate i -> Builder.int32Dec i
    Register r -> printRegister r
    Memory a -> printAddress printRegister a

printJmpOperand :: (v -> Builder) -> JmpOperand v -> Builder
printJmpOperand printRegister operand =
  case operand of
    JmpRelative Nothing offset -> ".+" <> Builder.int32Dec offset
    JmpRelative (Just label) 0 -> printLabel label
    JmpRelative (Just label) offset -> printLabel label <> "+" <> Builder.int32Dec offset
    JmpAbsolute o -> printOperand printRegister o

printLabel :: Label -> Builder
printLabel (Label.Label l) = ".L" <> Builder.byteString l

printAddress :: (v -> Builder) -> Address v -> Builder
printAddress printRegister (Address base maybeLabel disp) =
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
