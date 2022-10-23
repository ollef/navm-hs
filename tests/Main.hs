module Main where

import qualified BitSet
import qualified Hedgehog
import qualified Hedgehog.Main as Hedgehog
import qualified X86.Assembler

main :: IO ()
main =
  Hedgehog.defaultMain
    [ Hedgehog.checkParallel BitSet.tests
    , Hedgehog.checkParallel X86.Assembler.tests
    ]
