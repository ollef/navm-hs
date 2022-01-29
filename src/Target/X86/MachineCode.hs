module Target.X86.MachineCode where

import Data.Primitive.PrimArray
import Data.Word
import Prelude hiding (max, min)

newtype MachineCode = MachineCode (PrimArray Word8)
  deriving (Show)
