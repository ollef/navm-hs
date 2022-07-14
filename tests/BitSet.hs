{-# LANGUAGE OverloadedStrings #-}

module BitSet where

import Data.BitSet (BitSet)
import qualified Data.BitSet as BitSet
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.List (sort)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Target.X86 (Register)

genSets :: Hedgehog.Gen (BitSet Register, HashSet Register)
genSets = (\l -> (BitSet.fromList l, HashSet.fromList l)) <$> Gen.list (Range.linear 0 100) genRegister

genRegister :: Hedgehog.Gen Register
genRegister = Gen.enumBounded

tests :: Hedgehog.Group
tests =
  Hedgehog.Group
    "Data.BitSet"
    [
      ( "toList"
      , Hedgehog.property $ do
          (bs, hs) <- Hedgehog.forAll genSets
          sort (BitSet.toList bs) Hedgehog.=== sort (HashSet.toList hs)
      )
    ,
      ( "full"
      , Hedgehog.property $ do
          reg <- Hedgehog.forAll genRegister
          Hedgehog.assert $ BitSet.member reg BitSet.full
      )
    ,
      ( "insert-member"
      , Hedgehog.property $ do
          reg <- Hedgehog.forAll genRegister
          (bs, _) <- Hedgehog.forAll genSets
          Hedgehog.assert $ BitSet.member reg (BitSet.insert reg bs)
      )
    ,
      ( "delete-member"
      , Hedgehog.property $ do
          reg <- Hedgehog.forAll genRegister
          (bs, _) <- Hedgehog.forAll genSets
          Hedgehog.assert $ not $ BitSet.member reg (BitSet.delete reg bs)
      )
    ,
      ( "size"
      , Hedgehog.property $ do
          (bs, hs) <- Hedgehog.forAll genSets
          BitSet.size bs Hedgehog.=== HashSet.size hs
      )
    ,
      ( "union"
      , Hedgehog.property $ do
          (bs1, hs1) <- Hedgehog.forAll genSets
          (bs2, hs2) <- Hedgehog.forAll genSets
          sort (BitSet.toList (BitSet.union bs1 bs2)) Hedgehog.=== sort (HashSet.toList (HashSet.union hs1 hs2))
      )
    ,
      ( "intersection"
      , Hedgehog.property $ do
          (bs1, hs1) <- Hedgehog.forAll genSets
          (bs2, hs2) <- Hedgehog.forAll genSets
          sort (BitSet.toList (BitSet.intersection bs1 bs2)) Hedgehog.=== sort (HashSet.toList (HashSet.intersection hs1 hs2))
      )
    ,
      ( "difference"
      , Hedgehog.property $ do
          (bs1, hs1) <- Hedgehog.forAll genSets
          (bs2, hs2) <- Hedgehog.forAll genSets
          sort (BitSet.toList (BitSet.difference bs1 bs2)) Hedgehog.=== sort (HashSet.toList (HashSet.difference hs1 hs2))
      )
    ,
      ( "complement"
      , Hedgehog.property $ do
          (bs, _) <- Hedgehog.forAll genSets
          reg <- Hedgehog.forAll genRegister
          BitSet.member reg bs Hedgehog.=== not (BitSet.member reg (BitSet.complement bs))
      )
    ]
