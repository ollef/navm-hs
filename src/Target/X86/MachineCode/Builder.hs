{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Target.X86.MachineCode.Builder where

import ArrayBuilder (ArrayBuilder)
import qualified ArrayBuilder
import Control.Applicative
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
import Data.Semigroup
import Data.Tsil (Tsil)
import qualified Data.Tsil as Tsil
import Offset (Offset, offset)
import qualified Offset
import Prelude hiding (max, min)

newtype Builder s = Builder (Tsil (Part s))
  deriving (Monoid)

data Part s
  = Rigid (ArrayBuilder s)
  | Define !Label
  | FlexibleUse !Label (NonEmpty (ArrayBuilder s, LabelUse))
  | Use !Label !LabelUse

newtype Label = Label ByteString
  deriving (Eq, Show, Hashable)

data LabelUseSize = Int8 | Int32
  deriving (Show, Eq, Ord)

data LabelUse = LabelUse
  { positionOffset :: !Offset
  , writeOffset :: !Offset
  , size :: !LabelUseSize
  }
  deriving (Show)

instance Semigroup (Builder s) where
  Builder parts1 <> Builder Tsil.Empty = Builder parts1
  parts1 <> Builder (parts2 Tsil.:> part) = appendPart (parts1 <> Builder parts2) part

appendPart :: Builder s -> Part s -> Builder s
appendPart (Builder parts) part =
  Builder $
    case part of
      Rigid builder2 -> case parts of
        parts' Tsil.:> Rigid builder1 -> parts' Tsil.:> Rigid (builder1 <> builder2)
        _ -> parts Tsil.:> part
      _ -> parts Tsil.:> part

define :: Label -> Builder s
define label = Builder $ pure $ Define label

use :: Label -> LabelUseSize -> Offset -> Builder s
use label labelSize writeOffset =
  Builder $ pure $ Use label $ LabelUse {writeOffset, positionOffset = 0, size = labelSize}

data State = State
  { offsets :: !Offset.Flexible
  , definitions :: HashMap Label Offset.Flexible
  , rigidUses :: HashMap Label [(Offset, LabelUse)]
  }

rigidize :: [Part s] -> Builder s -> State -> Maybe ([Part s], State)
rigidize [] (Builder acc) state = Just (Foldable.toList acc, state)
rigidize (part : parts) acc state =
  case part of
    Rigid builder -> rigidize parts (appendPart acc part) state {offsets = offset (ArrayBuilder.size builder) $ offsets state}
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
    flexibleUsesOffset :: NonEmpty (ArrayBuilder s, a) -> Offset.Flexible
    flexibleUsesOffset uses = Offset.Flexible minSize maxSize
      where
        (Min minSize, Max maxSize) = foldMap (\(builder, _) -> (Min $ ArrayBuilder.size builder, Max $ ArrayBuilder.size builder)) uses

    selectFlexibleUse ::
      Label ->
      NonEmpty (ArrayBuilder s, LabelUse) ->
      Either (Maybe (NonEmpty (ArrayBuilder s, LabelUse))) (ArrayBuilder s, LabelUse)
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

selectAlternative :: Int -> [Part s] -> Builder s -> State -> Maybe ([Part s], State)
selectAlternative _alternative [] _acc _state = error "selectAlternative: unexpected end of input"
selectAlternative alternative (part : parts) acc state =
  case part of
    Rigid builder -> selectAlternative alternative parts (appendPart acc part) state {offsets = offset (ArrayBuilder.size builder) $ offsets state}
    Define _ -> error "selectAlternative: unexpected Define"
    Use _ _ -> error "selectAlternative: unexpected Use"
    FlexibleUse label uses -> do
      let (builder, selectedUse) = uses NonEmpty.!! alternative
      rigidize (Rigid builder : Use label selectedUse : parts) acc state

toArrayBuilder :: Builder s -> (ArrayBuilder s, HashMap Label Offset, HashMap Label [(Offset, LabelUse)])
toArrayBuilder (Builder initialParts) = go (Foldable.toList initialParts) State {offsets = mempty, definitions = mempty, rigidUses = mempty}
  where
    go :: [Part s] -> State -> (ArrayBuilder s, HashMap Label Offset, HashMap Label [(Offset, LabelUse)])
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
