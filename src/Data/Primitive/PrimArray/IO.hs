{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Primitive.PrimArray.IO where

import Data.Primitive
import Data.Word
import System.IO
import System.IO.Error

writeFile :: FilePath -> PrimArray Word8 -> IO ()
writeFile file arr =
  withBinaryFile file WriteMode \handle ->
    hPutBuf handle (primArrayContents arr) (sizeofPrimArray arr)

readFile :: FilePath -> IO (PrimArray Word8)
readFile file =
  withBinaryFile file ReadMode \handle -> do
    size <- fromIntegral <$> hFileSize handle
    arr <- newPinnedPrimArray size
    readBytes <- hGetBuf handle (mutablePrimArrayContents arr) size
    if readBytes == size
      then unsafeFreezePrimArray arr
      else ioError $ mkIOError eofErrorType "Data.Primitive.PrimArray.readFile" (Just handle) (Just file)
