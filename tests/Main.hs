{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
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
    instructions =
      [ Add
          (Address (Address' (Just R13) (Just (RAX, Scale1)) 0))
          (Immediate 0)
      , Add
          (Address (Address' (Just RBP) (Just (RAX, Scale1)) 0))
          (Immediate 0)
      ]

prop_x86Assembler :: Hedgehog.Property
prop_x86Assembler =
  Hedgehog.withTests 1000 $
    Hedgehog.property $ do
      instructions <- Hedgehog.forAll $ Gen.list (Range.linear 1 1000) generateInstruction
      matchGNUAssembler instructions

matchGNUAssembler :: [Instruction] -> Hedgehog.PropertyT IO ()
matchGNUAssembler instructions = do
  let assemblyCode =
        Builder.toLazyByteString $
          ".intel_syntax noprefix\n" <> printInstructions instructions
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
            ByteString.Lazy.readFile binaryFileName
  let assembledInstructions = buildMachineCode $ assembleInstructions instructions
  Hedgehog.annotate $ Char8.unpack assemblyCode
  assembledInstructions Hedgehog.=== MachineCode gnuAssembledInstructions
