module Target.X86.MachineCode where

import Data.Primitive.PrimArray
import Data.Word
import Text.Printf (printf)
import Prelude hiding (max, min)

newtype MachineCode = MachineCode (PrimArray Word8)

instance Show MachineCode where
  showsPrec _ (MachineCode arr) =
    foldrPrimArray (\c -> (showString (printf "%02x " c) .)) id arr
