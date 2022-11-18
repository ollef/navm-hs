{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Primitive.ByteArray.IO where

import Data.Primitive
import System.IO
import System.IO.Error

writeFile :: FilePath -> ByteArray -> IO ()
writeFile file arr =
  withBinaryFile file WriteMode \handle ->
    hPutBuf handle (byteArrayContents arr) (sizeofByteArray arr)

readFile :: FilePath -> IO ByteArray
readFile file =
  withBinaryFile file ReadMode \handle -> do
    size <- fromIntegral <$> hFileSize handle
    arr <- newPinnedByteArray size
    readBytes <- hGetBuf handle (mutableByteArrayContents arr) size
    if readBytes == size
      then unsafeFreezeByteArray arr
      else ioError $ mkIOError eofErrorType "Data.Primitive.ByteArray.readFile" (Just handle) (Just file)
