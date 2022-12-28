{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Data.HashSetList where

import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.HashSet.Extra as HashSet
import Data.Hashable

data HashSetList a = HashSetList !(HashSet a) [a]

instance Hashable a => Semigroup (HashSetList a) where
  HashSetList _ as <> b = foldr insert b as

instance Hashable a => Monoid (HashSetList a) where
  mempty = HashSetList mempty mempty

pattern Empty :: Hashable a => HashSetList a
pattern Empty <- HashSetList _ []
  where
    Empty = mempty

pattern (:<) :: Hashable a => a -> HashSetList a -> HashSetList a
pattern a :< as <- (popFront -> Just (a, as))
  where
    a :< as = insert a as

fromList :: Hashable a => [a] -> HashSetList a
fromList = foldr insert mempty

toList :: HashSetList a -> [a]
toList (HashSetList _ as) = as

insert :: Hashable a => a -> HashSetList a -> HashSetList a
insert a hsl@(HashSetList s as)
  | alreadyMember = hsl
  | otherwise = HashSetList s' (a : as)
  where
    (s', alreadyMember) = HashSet.insertMember a s

popFront :: Hashable a => HashSetList a -> Maybe (a, HashSetList a)
popFront (HashSetList _ []) = Nothing
popFront (HashSetList s (a : as)) = Just (a, HashSetList (HashSet.delete a s) as)
