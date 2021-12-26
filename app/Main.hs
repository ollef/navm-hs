{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Block
import Data.Functor.Identity
import Graph
import Openness

data Instr (i :: OC) (o :: OC) where
  Init :: String -> Instr 'C 'O
  Add :: Instr 'O 'O
  Branch :: String -> Instr 'O 'C

deriving instance Show (Instr i o)

instance Labelled Instr where
  type Label Instr = String
  label (Init l) = l

test :: Block Instr 'C 'C
test = BlockCC (Init "loop") [Add] (Branch "loop")

test2 :: Block Instr 'O 'C
test2 = BlockOC [Add] (Branch "loop")

test3 :: Graph (Block Instr) 'C 'C
test3 = Openness.foldMap go test
  where
    go :: Instr i o -> Graph (Block Instr) i o
    go instr@(Init {}) = unit $ unit instr
    go Add = unit (unit $ Branch "a") `append` unit (unit (Init "a") `append` unit Add)
    go instr@(Branch {}) = unit $ unit instr

test4 :: Graph (Block Instr) 'O 'C
test4 = Openness.foldMap go test2
  where
    go :: Instr i o -> Graph (Block Instr) i o
    go instr@(Init {}) = unit $ unit instr
    go Add = unit (unit $ Branch "a") `append` unit (unit (Init "a") `append` unit Add)
    go instr@(Branch {}) = unit $ unit instr

monadicTest :: Identity (Graph (Block Instr) 'O 'C)
monadicTest = Openness.foldMapA go test2
  where
    go :: Instr i o -> Identity (Graph (Block Instr) i o)
    go instr@(Init {}) = pure $ unit $ unit instr
    go Add = pure $ unit (unit $ Branch "a") `append` unit (unit (Init "a") `append` unit Add)
    go instr@(Branch {}) = pure $ unit $ unit instr

main :: IO ()
main = do
  putStrLn "hello world"
