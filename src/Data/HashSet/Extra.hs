module Data.HashSet.Extra where

import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable
import Data.Maybe

insertMember :: Hashable a => a -> HashSet a -> (HashSet a, Bool)
insertMember a s = (HashSet.fromMap m, member)
  where
    (member, m) =
      HashMap.alterF
        ( \maybeExisting ->
            (isJust maybeExisting, Just ())
        )
        a
        $ HashSet.toMap s
