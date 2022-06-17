{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Data.BitSet where

import Data.Bits
import Data.Foldable (foldl')
import GHC.Exts
import Prelude hiding (null, words)

newtype BitSet a = BitSet Integer
  deriving (Eq, Ord)

instance Enum a => IsList (BitSet a) where
  type Item (BitSet a) = a
  toList = Data.BitSet.toList
  fromList = Data.BitSet.fromList

instance (Enum a, Show a) => Show (BitSet a) where
  showsPrec p xs =
    showParen (p > 10) $
      showString "fromList " . shows (Data.BitSet.toList xs)

instance Semigroup (BitSet a) where
  (<>) = union

instance Monoid (BitSet a) where
  mempty = BitSet 0

empty :: BitSet a
empty = BitSet 0

full :: forall a. (Enum a, Bounded a) => BitSet a
full = BitSet $ (1 `shiftL` fromEnum (maxBound :: a)) - 1

singleton :: Enum a => a -> BitSet a
singleton = BitSet . bit . fromEnum

fromList :: Enum a => [a] -> BitSet a
fromList = foldl' (flip insert) mempty

toList :: Enum a => BitSet a -> [a]
toList (BitSet i) = go 0 $ integerWords i
  where
    go :: Enum a => Int -> [Word] -> [a]
    go _ [] = []
    go offset (w : ws) = go' offset (finiteBitSize w) w ws
    go' :: Enum a => Int -> Int -> Word -> [Word] -> [a]
    go' offset bitsLeft word rest
      | trailingZeros == finiteBitSize word = go (offset + bitsLeft) rest
      | bitsLeft == 0 = go offset rest
      | otherwise = do
          let firstSet = trailingZeros + 1
          toEnum (offset + trailingZeros)
            : go'
              (offset + firstSet)
              (bitsLeft - firstSet)
              (word `shiftR` firstSet)
              rest
      where
        trailingZeros = countTrailingZeros word

insert :: Enum a => a -> BitSet a -> BitSet a
insert a (BitSet i) = BitSet $ setBit i $ fromEnum a

delete :: Enum a => a -> BitSet a -> BitSet a
delete a (BitSet i) = BitSet $ clearBit i $ fromEnum a

null :: BitSet a -> Bool
null (BitSet i) = i == 0

member :: Enum a => a -> BitSet a -> Bool
member a (BitSet i) = testBit i $ fromEnum a

size :: BitSet a -> Int
size (BitSet i) = popCount i

union :: BitSet a -> BitSet a -> BitSet a
union (BitSet i) (BitSet j) = BitSet $ i .|. j

intersection :: BitSet a -> BitSet a -> BitSet a
intersection (BitSet i) (BitSet j) = BitSet $ i .&. j

difference :: BitSet a -> BitSet a -> BitSet a
difference (BitSet i) (BitSet j) = BitSet $ i `xor` j

integerWords :: Integer -> [Word]
integerWords 0 = []
integerWords x = fromIntegral x : integerWords (x `shiftR` finiteBitSize (undefined :: Word))

complement :: (Enum a, Bounded a) => BitSet a -> BitSet a
complement = difference full

uncons :: Enum a => BitSet a -> Maybe (a, BitSet a)
uncons bs = case Data.BitSet.toList bs of
  [] -> Nothing
  a : _ -> Just (a, delete a bs)

pattern Empty :: BitSet a
pattern Empty <- (null -> True)
  where
    Empty = empty

pattern (:<) :: Enum a => a -> BitSet a -> BitSet a
pattern a :< as <- (uncons -> Just (a, as))
  where
    a :< as = insert a as

{-# COMPLETE Empty, (:<) #-}
