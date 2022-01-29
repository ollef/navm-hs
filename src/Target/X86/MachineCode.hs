{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Target.X86.MachineCode where

import Control.Applicative
import Control.Monad.ST
import Data.ByteString.Internal as ByteString.Internal
import Data.Foldable
import qualified Data.Foldable as Foldable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Hashable
import Data.Int
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Data.Primitive.PrimArray
import Data.Primitive.Ptr
import Data.Semigroup
import Data.Tsil (Tsil)
import qualified Data.Tsil as Tsil
import Data.Word
import GHC.Exts hiding (build)
import GHC.ST
import Offset (Offset, offset)
import qualified Offset
import Prelude hiding (max, min)

newtype MachineCode = MachineCode (PrimArray Word8)
  deriving (Show)

newtype Label = Label ByteString
  deriving (Eq, Show, Hashable)

newtype Size = Size Int
  deriving (Show, Eq, Ord, Num, Real, Enum, Integral)

data LabelUseSize = Int8 | Int32
  deriving (Show, Eq, Ord)

data LabelUse = LabelUse
  { positionOffset :: !Offset
  , writeOffset :: !Offset
  , size :: !LabelUseSize
  }
  deriving (Show)

data Builder s = Builder
  { size :: !Size
  , function :: !((# State# s, Addr# #) -> State# s)
  }

stBuilder ::
  Size ->
  (Ptr Word8 -> ST s ()) ->
  Builder s
stBuilder bytes f =
  Builder bytes $ \(# s, addr #) -> do
    let (ST st) = f (Ptr addr)
    let !(# s', () #) = st s
    s'

runBuilder :: (forall s. Builder s) -> MachineCode
runBuilder builder =
  runST $ do
    let Builder {size, function} = builder
    arr <- newPinnedPrimArray $ fromIntegral size
    let !(Ptr startAddr) = mutablePrimArrayContents arr
    ST
      ( \s -> do
          let !s' = function (# s, startAddr #)
          (# s', () #)
      )
    frozenArr <- unsafeFreezePrimArray arr
    pure $ MachineCode frozenArr

instance Semigroup (Builder m) where
  Builder size1 f <> Builder size2 g =
    Builder
      (size1 + size2)
      ( \x@(# _, addr #) -> do
          let !s = f x
              !(Ptr addr') = advancePtr (Ptr addr :: Ptr Word8) (coerce size1)
          g (# s, addr' #)
      )

instance Monoid (Builder m) where
  mempty = Builder 0 (\(# s, _ #) -> s)
  mconcat bs =
    Builder
      ( foldl'
          (\n (Builder size _) -> n + size)
          0
          bs
      )
      ( snd $
          foldl'
            ( \(!size1, !f) (Builder size2 g) ->
                ( size1 + size2
                , \x@(# _, addr #) -> do
                    let !s = f x
                        !(Ptr addr') = advancePtr (Ptr addr :: Ptr Word8) (coerce size1)
                    g (# s, addr' #)
                )
            )
            (0, \(# s, _ #) -> s)
            bs
      )

data FlexibleBuilderPart s
  = Rigid (Builder s)
  | Define !Label
  | FlexibleUse !Label (NonEmpty (Builder s, LabelUse))
  | Use !Label !LabelUse

newtype FlexibleBuilder s = FlexibleBuilder (Tsil (FlexibleBuilderPart s))
  deriving (Monoid)

instance Semigroup (FlexibleBuilder s) where
  FlexibleBuilder parts1 <> FlexibleBuilder Tsil.Empty = FlexibleBuilder parts1
  parts1 <> FlexibleBuilder (parts2 Tsil.:> part) = appendPart (parts1 <> FlexibleBuilder parts2) part

appendPart :: FlexibleBuilder s -> FlexibleBuilderPart s -> FlexibleBuilder s
appendPart (FlexibleBuilder parts) part =
  FlexibleBuilder $
    case part of
      Rigid builder2 -> case parts of
        parts' Tsil.:> Rigid builder1 -> parts' Tsil.:> Rigid (builder1 <> builder2)
        _ -> parts Tsil.:> part
      _ -> parts Tsil.:> part

define :: Label -> FlexibleBuilder s
define label = FlexibleBuilder $ pure $ Define label

use :: Label -> LabelUseSize -> Offset -> FlexibleBuilder s
use label labelSize writeOffset =
  FlexibleBuilder $ pure $ Use label $ LabelUse {writeOffset, positionOffset = 0, size = labelSize}

data State = State
  { offsets :: !Offset.Flexible
  , definitions :: HashMap Label Offset.Flexible
  , rigidUses :: HashMap Label [(Offset, LabelUse)]
  }

rigidize :: [FlexibleBuilderPart s] -> FlexibleBuilder s -> State -> Maybe ([FlexibleBuilderPart s], State)
rigidize [] (FlexibleBuilder acc) state = Just (Foldable.toList acc, state)
rigidize (part : parts) acc state =
  case part of
    Rigid Builder {size} -> rigidize parts (appendPart acc part) state {offsets = offset (coerce size) $ offsets state}
    Define label
      | Just _ <- Offset.rigid $ offsets state -> rigidize parts acc state {definitions = HashMap.insert label (offsets state) $ definitions state}
      | otherwise -> rigidize parts (appendPart acc part) state {definitions = HashMap.insert label (offsets state) $ definitions state}
    Use label labelUse
      | Just o <- Offset.rigid $ offsets state -> rigidize parts acc state {rigidUses = HashMap.insertWith (<>) label [(o, labelUse)] $ rigidUses state}
      | otherwise -> rigidize parts (appendPart acc part) state
    FlexibleUse label uses ->
      case selectFlexibleUse label uses of
        Left Nothing -> Nothing
        Left (Just uses') -> rigidize parts (appendPart acc $ FlexibleUse label uses') state {offsets = flexibleUsesOffset uses' <> offsets state}
        Right (builder, selectedUse) -> rigidize (Rigid builder : Use label selectedUse : parts) acc state
  where
    flexibleUsesOffset :: NonEmpty (Builder s, a) -> Offset.Flexible
    flexibleUsesOffset uses = Offset.Flexible minSize maxSize
      where
        (Min minSize, Max maxSize) = foldMap (\(Builder {size}, _) -> (Min $ coerce size, Max $ coerce size)) uses

    selectFlexibleUse :: Label -> NonEmpty (Builder s, LabelUse) -> Either (Maybe (NonEmpty (Builder s, LabelUse))) (Builder s, LabelUse)
    selectFlexibleUse label uses =
      case HashMap.lookup label $ definitions state of
        Nothing -> Left $ Just uses
        Just definition -> do
          let possiblyValidUses = NonEmpty.filter (possiblyValid definition . snd) uses
          case filter (alwaysValid definition . snd) possiblyValidUses of
            [] -> case possiblyValidUses of
              [] -> Left Nothing
              [possiblyValidUse] -> Right possiblyValidUse
              possiblyValidUse : possiblyValidUses' -> Left $ Just $ possiblyValidUse NonEmpty.:| possiblyValidUses'
            selectedUse : _ -> Right selectedUse

    alwaysValid :: Offset.Flexible -> LabelUse -> Bool
    alwaysValid definition LabelUse {size = useSize, positionOffset = useOffset} =
      minUseBound <= relativeMin && relativeMax <= maxUseBound
      where
        (relativeMin, relativeMax) = relativeOffsets definition $ offset useOffset $ offsets state
        (minUseBound, maxUseBound) = useBounds useSize

    possiblyValid :: Offset.Flexible -> LabelUse -> Bool
    possiblyValid definition LabelUse {size = useSize, positionOffset = useOffset} =
      minUseBound <= relativeMin && relativeMin <= maxUseBound
        || minUseBound <= relativeMax && relativeMax <= maxUseBound
      where
        (relativeMin, relativeMax) = relativeOffsets definition $ offset useOffset $ offsets state
        (minUseBound, maxUseBound) = useBounds useSize

    relativeOffsets (Offset.Flexible defMin defMax) (Offset.Flexible useMin useMax) =
      (relativeMin, relativeMax)
      where
        (Min relativeMin, Max relativeMax) = foldMap (\ !r -> (Min r, Max r)) rs
        rs = [d - u | d <- [defMin, defMax], u <- [useMin, useMax]]

    useBounds useSize =
      case useSize of
        Int8 -> (fromIntegral (minBound :: Int8), fromIntegral (maxBound :: Int8))
        Int32 -> (fromIntegral (minBound :: Int32), fromIntegral (maxBound :: Int32))

selectAlternative :: Int -> [FlexibleBuilderPart s] -> FlexibleBuilder s -> State -> Maybe ([FlexibleBuilderPart s], State)
selectAlternative _alternative [] _acc _state = error "selectAlternative: unexpected end of input"
selectAlternative alternative (part : parts) acc state =
  case part of
    Rigid Builder {size} -> selectAlternative alternative parts (appendPart acc part) state {offsets = offset (coerce size) $ offsets state}
    Define _ -> error "selectAlternative: unexpected Define"
    Use _ _ -> error "selectAlternative: unexpected Use"
    FlexibleUse label uses -> do
      let (builder, selectedUse) = uses NonEmpty.!! alternative
      rigidize (Rigid builder : Use label selectedUse : parts) acc state

toBuilder :: FlexibleBuilder s -> (Builder s, HashMap Label Offset, HashMap Label [(Offset, LabelUse)])
toBuilder (FlexibleBuilder initialParts) = go (Foldable.toList initialParts) State {offsets = mempty, definitions = mempty, rigidUses = mempty}
  where
    go :: [FlexibleBuilderPart s] -> State -> (Builder s, HashMap Label Offset, HashMap Label [(Offset, LabelUse)])
    go [] state = (mempty, (\(Offset.Flexible o _) -> o) <$> definitions state, rigidUses state)
    go [Rigid builder] state = (builder, (\(Offset.Flexible o _) -> o) <$> definitions state, rigidUses state)
    go parts state = case rigidize parts mempty state {offsets = mempty} of
      Nothing -> error "toBuilder: impossible"
      Just (parts', state')
        | offsets state == offsets state' ->
          -- We're not making any progress: see if selecting alternatives in
          -- the first flexible use gets us unstuck.
          case catMaybes [selectAlternative alternative parts' mempty state' {offsets = mempty} | alternative <- [0 ..]] of
            [] -> error "toBuilder: no alternative works"
            (parts'', state'') : _ -> go parts'' state''
        | otherwise -> go parts' state'
