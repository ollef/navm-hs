{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NamedFieldPuns #-}

module Target.X86.Register.Class where

import Data.Bits
import Data.Word
import Target.X86.Register
import Prelude hiding (any)

data Class = Class {mask :: !Word8, pattern :: !Word8}

exact :: Register -> Class
exact reg = Class {mask = 0b1111, pattern = fromEnum8 reg}

any :: Class
any = Class {mask = 0, pattern = 0}

contains :: Register -> Class -> Bool
contains reg Class {mask, pattern} = fromEnum8 reg .&. mask == pattern

registers :: Class -> [Register]
registers Class {mask, pattern} = do
  b3 <- b 3
  b2 <- b 2
  b1 <- b 1
  b0 <- b 0
  pure $ toEnum $ fromIntegral $ b3 .|. b2 .|. b1 .|. b0
  where
    b i
      | testBit mask i = [bit i .&. pattern]
      | otherwise = [0, bit i]

fromEnum8 :: Enum a => a -> Word8
fromEnum8 x =
  fromIntegral (fromEnum x)

instance FromRegister Class where
  fromRegister = exact
