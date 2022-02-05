{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Main as Hedgehog
import qualified Hedgehog.Range as Range
import System.IO (hClose)
import System.IO.Temp (withTempFile)
import System.Process (callProcess)
import Target.X86.Assembler
import Target.X86.Assembly
import qualified Target.X86.MachineCode as MachineCode
import qualified Target.X86.MachineCode.Builder as MachineCode.Builder
import Target.X86.Printer
import Target.X86.Random

main :: IO ()
main =
  Hedgehog.defaultMain
    [ Hedgehog.checkParallel $
        Hedgehog.Group
          "X86"
          [ ("Assembler matches 'GNU as' on selected instructions", test_x86Assembler)
          , ("Assembler matches 'GNU as' on random instructions", prop_x86Assembler)
          ]
    ]

test_x86Assembler :: Hedgehog.Property
test_x86Assembler =
  Hedgehog.withTests 1 $
    Hedgehog.property $
      forM_ instructions $ \instruction -> do
        Hedgehog.annotateShow instruction
        matchGNUAssembler [instruction]
  where
    instructions :: [Instruction Register]
    instructions =
      [ add [r13 + rax] [r13 + rax] 0
      , add [rbp + rax] [rbp + rax] 0
      , add [rbp + rax * 4] [rbp + rax * 4] 0
      , add [r13 + rax * 4] [r13 + rax * 4] 0
      ]

prop_x86Assembler :: Hedgehog.Property
prop_x86Assembler =
  Hedgehog.withTests 1000 $
    Hedgehog.property $ do
      instructions <- Hedgehog.forAll $ Gen.list (Range.linear 1 1000) $ generateInstruction []
      matchGNUAssembler instructions

matchGNUAssembler :: [Instruction Register] -> Hedgehog.PropertyT IO ()
matchGNUAssembler instructions = do
  let assemblyCode =
        Builder.toLazyByteString $
          ".intel_syntax noprefix\n" <> printInstructions instructions
  Hedgehog.annotate $ Char8.unpack assemblyCode
  gnuAssembledInstructionsBS <-
    Hedgehog.evalIO $
      withTempFile "." "tmp.s" $ \assemblyFileName assemblyFileHandle ->
        withTempFile "." "tmp.o" $ \objectFileName objectFileHandle -> do
          withTempFile "." "tmp.bin" $ \binaryFileName binaryFileHandle -> do
            hClose assemblyFileHandle
            hClose objectFileHandle
            hClose binaryFileHandle
            ByteString.Lazy.writeFile assemblyFileName assemblyCode
            callProcess "as" [assemblyFileName, "-o", objectFileName]
            callProcess "objcopy" ["-O", "binary", "-j", ".text", objectFileName, binaryFileName]
            ByteString.readFile binaryFileName
  let assembledInstructions = MachineCode.Builder.run $ assembleInstructions instructions
  gnuAssembledInstructions <- Hedgehog.evalIO $ MachineCode.fromByteString gnuAssembledInstructionsBS
  assembledInstructions Hedgehog.=== gnuAssembledInstructions
