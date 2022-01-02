{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Main as Hedgehog
import qualified Hedgehog.Range as Range
import System.IO (hClose)
import System.IO.Temp (withTempFile)
import System.Process (callProcess)
import Target.X86.Assembler
import Target.X86.Printer
import Target.X86.Random

main :: IO ()
main =
  Hedgehog.defaultMain
    [ Hedgehog.checkParallel $
        Hedgehog.Group
          "X86"
          [("Assembler matches GNU as", prop_x86Assembler)]
    ]

prop_x86Assembler :: Hedgehog.Property
prop_x86Assembler = Hedgehog.property $ do
  instructions <- Hedgehog.forAll $ Gen.list (Range.linear 1 1000) generateInstruction
  gnuAssembledInstructions <-
    Hedgehog.evalIO $
      withTempFile "." "tmp.s" $ \assemblyFileName assemblyFileHandle ->
        withTempFile "." "tmp.o" $ \objectFileName objectFileHandle -> do
          withTempFile "." "tmp.bin" $ \binaryFileName binaryFileHandle -> do
            hClose assemblyFileHandle
            hClose objectFileHandle
            hClose binaryFileHandle
            ByteString.Lazy.writeFile assemblyFileName $
              Builder.toLazyByteString $
                ".intel_syntax noprefix\n" <> printInstructions instructions
            callProcess "as" [assemblyFileName, "-o", objectFileName]
            callProcess "objcopy" ["-O", "binary", "-j", ".text", objectFileName, binaryFileName]
            ByteString.Lazy.readFile binaryFileName
  let assembledInstructions = buildMachineCode $ assembleInstructions instructions
  assembledInstructions Hedgehog.=== MachineCode gnuAssembledInstructions
