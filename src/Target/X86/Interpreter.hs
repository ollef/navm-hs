{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Target.X86.Interpreter where

import Data.Bits
import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EnumMap
import Data.Foldable
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.WideWord.Word128 (Word128 (Word128))
import Data.Word
import Target.X86.Assembly as X86
import Prelude hiding (read)

data State register = State
  { registers :: !(EnumMap register Word64)
  , memory :: !(IntMap Word8)
  }

interpret :: Enum register => State register -> X86.Instruction register -> State register
interpret state instruction = case instruction of
  X86.Add dst src1 src2 -> do
    let result = inputOperand src1 + inputOperand src2
    outputOperand dst result state
  X86.Mul (dst1, dst2) src1 src2 -> do
    let (low, high) = mul128 (readRegister src1 state) (inputOperand src2)
    writeRegister dst1 high $ writeRegister dst2 low state
  X86.Jmp _ -> error "interpret jmp"
  X86.Call _ -> error "interpret call"
  X86.Ret -> error "interpret ret"
  X86.Int _ -> error "interpret int"
  X86.Mov dst src -> outputOperand dst (inputOperand src) state
  X86.MovImmediate64 dst src -> writeRegister dst (fromIntegral src) state
  X86.Define _ -> state
  where
    inputOperand operand = case operand of
      X86.Immediate imm -> fromIntegral imm
      X86.Register reg -> readRegister reg state
      X86.Memory addr -> readMemory (address addr) state
    address (X86.Address b l i) =
      base b + label l + fromIntegral i
      where
        base Relative = error "relative base"
        base (Absolute maybeBase maybeRegScale) =
          maybe 0 (`readRegister` state) maybeBase
            + maybe 0 (\(reg, scale) -> readRegister reg state * X86.fromScale scale) maybeRegScale
        label Nothing = 0
        label (Just _) = error "TODO address label"

    outputOperand operand value state' = case operand of
      X86.Immediate {} -> error "immediate output operand"
      X86.Register reg -> writeRegister reg value state'
      X86.Memory addr -> writeMemory (address addr) value state'

readRegister :: Enum register => register -> State register -> Word64
readRegister reg state = EnumMap.findWithDefault 0 reg state.registers

writeRegister :: Enum register => register -> Word64 -> State register -> State register
writeRegister reg value state =
  state
    { registers = EnumMap.insert reg value state.registers
    }

readMemory :: Word64 -> State register -> Word64
readMemory address state =
  foldl'
    (.|.)
    0
    [ shiftL
      (fromIntegral $ IntMap.findWithDefault 0 (fromIntegral $ address + fromIntegral i) state.memory)
      (i * 8)
    | i <- [0 .. 7]
    ]

writeMemory :: Word64 -> Word64 -> State register -> State register
writeMemory address value state =
  state
    { memory =
        foldl'
          (\memory i -> IntMap.insert (fromIntegral $ address + fromIntegral i) (fromIntegral $ shiftR value (i * 8)) memory)
          state.memory
          [0 .. 7]
    }

mul128 :: Word64 -> Word64 -> (Word64, Word64)
mul128 a b = (low, high)
  where
    (Word128 high low) = fromIntegral a * fromIntegral b
