{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Target.X86.RegisterAllocation2 where

import qualified Block
import Data.Foldable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import qualified Graph
import Openness (Label, OC (..))
import qualified Register
import qualified Target.X86.Assembly as X86
import qualified Target.X86.Constraints as Constraints

data Instruction i o where
  Initiator :: Instruction 'C 'O
  Instruction :: X86.Instruction Register.Virtual -> Instruction 'O 'O
  Terminator :: Instruction 'O 'C

type Block = Block.Block Instruction

type Graph = Graph.Graph Block

definitions :: Graph 'O 'C -> HashMap Register.Virtual (Maybe (Label Block), Int)
definitions (Graph.OC (Block.OC entryBlock Terminator) blocks) =
  HashMap.fromList
    [ (def, (Nothing, i))
    | (i, Instruction instr) <- zip [0 ..] entryBlock
    , def <-
        fold $
          Constraints.mapWithClass
            ( \occ _ reg -> case occ of
                Constraints.Definition -> pure reg
                Constraints.Use -> mempty
            )
            instr
    ]
    <> HashMap.fromList
      ( concat
          [ [ (def, (Just label, i))
            | (i, Instruction instr) <- zip [1 ..] instructions
            , def <-
                fold $
                  Constraints.mapWithClass
                    ( \occ _ reg -> case occ of
                        Constraints.Definition -> pure reg
                        Constraints.Use -> mempty
                    )
                    instr
            ]
          | (label, Block.CC Initiator instructions Terminator) <- HashMap.toList blocks
          ]
      )
