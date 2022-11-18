{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module X86.Assembler where

import qualified ArrayBuilder
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Primitive.PrimArray.IO as PrimArray
import Data.String
import qualified Hedgehog
import System.IO (hClose)
import System.IO.Temp (withTempFile)
import System.Process (callProcess)
import Target.X86.Assembler
import Target.X86.Assembly
import qualified Target.X86.MachineCode as MachineCode
import Target.X86.Printer
import Target.X86.Random

tests :: Hedgehog.Group
tests =
  Hedgehog.Group
    "X86"
    $ assembleSelectedX86Instructions
      <> assembleRandomX86Instructions

assembleSelectedX86Instructions :: [(Hedgehog.PropertyName, Hedgehog.Property)]
assembleSelectedX86Instructions =
  [ ( "Selected instructions " <> fromString (show testNumber)
    , Hedgehog.withTests 1 $
        Hedgehog.property $ do
          Hedgehog.annotateShow is
          matchGNUAssembler is
    )
  | (testNumber, is) <- zip [0 :: Int ..] instructions
  ]
  where
    instructions :: [[Instruction Register]]
    instructions =
      [ add [r13 + rax] [r13 + rax] 0
      , add [rbp + rax] [rbp + rax] 0
      , add [rbp + rax * 4] [rbp + rax * 4] 0
      , add [r13 + rax * 4] [r13 + rax * 4] 0
      , [define "a", add rax rax 0, add [rip + "a"] [rip + "a"] 0]
      , [define "a", add rax rax 0, add [rip + "a" + 1] [rip + "a" + 1] 0]
      , [define "a", add rax rax 0, add [rip + "a" - 1] [rip + "a" - 1] 0]
      , [jmp "a", define "a"]
      , [define "a", jmp "a"]
      , [define "a", jmp ("a" + 1)]
      , jmp rax
      , call rax
      , [define "a", add rax rax 0, jmp $ "a" - 123]
      , [add rax rax 0, define "a", jmp $ "a" + 130]
      ]

assembleRandomX86Instructions :: [(Hedgehog.PropertyName, Hedgehog.Property)]
assembleRandomX86Instructions =
  [
    ( "Random instructions"
    , Hedgehog.withTests 1000 $
        Hedgehog.property $ do
          instructions <- Hedgehog.forAll generateInstructions
          matchGNUAssembler instructions
    )
  ]

matchGNUAssembler :: [Instruction Register] -> Hedgehog.PropertyT IO ()
matchGNUAssembler instructions = do
  let assemblyCode =
        Builder.toLazyByteString $
          ".intel_syntax noprefix\n" <> printInstructions instructions
  Hedgehog.annotate $ Char8.unpack assemblyCode
  gnuAssembledInstructions <-
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
            PrimArray.readFile binaryFileName
  let assembledInstructions = ArrayBuilder.run $ MachineCode.run $ assembleInstructions instructions
  assembledInstructions Hedgehog.=== gnuAssembledInstructions
