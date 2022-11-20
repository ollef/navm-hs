module Main where

import qualified BitSet
import qualified Hedgehog
import qualified Hedgehog.Main as Hedgehog
import qualified X86.Assembler
import qualified X86.RegisterAllocation

main :: IO ()
main =
  Hedgehog.defaultMain
    [ Hedgehog.checkParallel X86.RegisterAllocation.tests
    , Hedgehog.checkParallel BitSet.tests
    , Hedgehog.checkParallel X86.Assembler.tests
    ]
