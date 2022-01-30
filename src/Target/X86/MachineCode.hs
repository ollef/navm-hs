{-# LANGUAGE BangPatterns #-}

module Target.X86.MachineCode where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as ByteString
import Data.Primitive.PrimArray
import Data.Primitive.Ptr
import Data.Word
import Text.Printf (printf)
import Prelude hiding (max, min)

newtype MachineCode = MachineCode (PrimArray Word8)

instance Show MachineCode where
  showsPrec _ (MachineCode arr) =
    foldrPrimArray (\c -> (showString (printf "%02x " c) .)) id arr

toByteString :: MachineCode -> IO ByteString
toByteString (MachineCode arr) = ByteString.unsafePackAddressLen (sizeofPrimArray arr) addr
  where
    !(Ptr addr) = primArrayContents arr
