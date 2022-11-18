{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoFieldSelectors #-}

module Target.X86.MachineCode where

import ArrayBuilder (ArrayBuilder)
import qualified ArrayBuilder
import Control.Applicative
import Data.Coerce
import Data.Foldable
import qualified Data.Foldable as Foldable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Int
import Data.Maybe
import Data.Semigroup
import Data.Tsil (Tsil)
import qualified Data.Tsil as Tsil
import Data.Word
import Label
import Offset (Offset, offset)
import qualified Offset
import Prelude hiding (max, min)
import qualified Prelude

newtype Builder = Builder (Tsil Part)
  deriving (Monoid)

data Part
  = Rigid ArrayBuilder
  | Flexible [Part] [Part]
  | Define !Label
  | Use !Label !LabelUse

data LabelUseSize = Int8 | Int32
  deriving (Show, Eq, Ord)

useSizeBytes :: LabelUseSize -> Offset
useSizeBytes Int8 = 1
useSizeBytes Int32 = 4

data LabelUse = LabelUse
  { writeOffset :: !Offset
  , size :: !LabelUseSize
  , displacement :: !Int
  }
  deriving (Show)

instance Semigroup Builder where
  Builder parts1 <> Builder Tsil.Empty = Builder parts1
  parts1 <> Builder (parts2 Tsil.:> part) = appendPart (parts1 <> Builder parts2) part

appendPart :: Builder -> Part -> Builder
appendPart (Builder parts) part =
  Builder $
    case part of
      Rigid builder2 -> case parts of
        parts' Tsil.:> Rigid builder1 -> parts' Tsil.:> Rigid (builder1 <> builder2)
        _ -> parts Tsil.:> part
      _ -> parts Tsil.:> part

appendParts :: Builder -> [Part] -> Builder
appendParts = foldl' appendPart

define :: Label -> Builder
define label = Builder $ pure $ Define label

useRelativeToEnd :: Label -> LabelUseSize -> Int -> Builder
useRelativeToEnd label labelSize displacement =
  Builder [Rigid $ ArrayBuilder.skip byteSize, Use label $ LabelUse {writeOffset = -byteSize, size = labelSize, displacement}]
  where
    byteSize = useSizeBytes labelSize

data Valid = Possibly | Always
  deriving (Eq, Ord, Show)

instance Semigroup Valid where
  (<>) = Prelude.min

instance Monoid Valid where
  mempty = Always

data State = State
  { offset :: !Offset.Flexible
  , definitions :: HashMap Label Offset.Flexible
  , rigidUses :: HashMap Label [(Offset, LabelUse)]
  , flexibleUseCount :: !Int
  , valid :: !Valid
  }

rewind :: State -> State
rewind state =
  State
    { offset = mempty
    , definitions = state.definitions
    , rigidUses = state.rigidUses
    , flexibleUseCount = 0
    , valid = mempty
    }

rigidize :: [Part] -> Builder -> State -> Maybe ([Part], State)
rigidize [] (Builder acc) state = Just (Foldable.toList acc, state)
rigidize (part : parts) acc state =
  case part of
    Rigid builder -> rigidize parts (appendPart acc part) state {offset = offset (ArrayBuilder.size builder) state.offset}
    Define label
      | Just _ <- Offset.rigid state.offset -> rigidize parts acc state {definitions = HashMap.insert label state.offset state.definitions}
      | otherwise -> rigidize parts (appendPart acc part) state {definitions = HashMap.insert label state.offset state.definitions}
    Use label labelUse ->
      case HashMap.lookup label state.definitions of
        Nothing -> rigidize parts (appendPart acc part) state {valid = Possibly <> state.valid}
        Just definition
          | alwaysValid definition labelUse
          , Just o <- Offset.rigid state.offset ->
              rigidize parts acc state {rigidUses = HashMap.insertWith (<>) label [(o, labelUse)] state.rigidUses}
          | possiblyValid definition labelUse -> rigidize parts (appendPart acc part) state {valid = Possibly <> state.valid}
          | otherwise -> Nothing
    Flexible parts1 parts2 ->
      case (rigidize parts1 mempty state {valid = mempty}, rigidize parts2 mempty state {valid = state.valid}) of
        (Nothing, Nothing) -> Nothing
        (Nothing, Just (parts2', state2)) -> rigidize parts (appendParts acc parts2') state2
        (Just (parts1', state1@State {valid = Possibly}), Just (parts2', state2)) -> rigidize parts (appendPart acc $ Flexible parts1' parts2') state'
          where
            state' =
              state
                { offset = Offset.choice state1.offset state2.offset
                , valid = state2.valid
                , flexibleUseCount = 1 + Prelude.max state1.flexibleUseCount state2.flexibleUseCount
                }
        (Just (parts1', state1@State {valid = Possibly}), Nothing) -> rigidize parts (appendParts acc parts1') state1
        (Just (parts1', state1@State {valid = Always}), _) -> rigidize parts (appendParts acc parts1') state1 {valid = state.valid}
  where
    alwaysValid :: Offset.Flexible -> LabelUse -> Bool
    alwaysValid definition LabelUse {size = useSize, displacement} =
      minUseBound <= relativeMin && relativeMax <= maxUseBound
      where
        (relativeMin, relativeMax) = relativeOffsets (Offset.offset (coerce displacement) definition) state.offset
        (minUseBound, maxUseBound) = useBounds useSize

    possiblyValid :: Offset.Flexible -> LabelUse -> Bool
    possiblyValid definition LabelUse {size = useSize, displacement} =
      minUseBound <= relativeMin && relativeMin <= maxUseBound
        || minUseBound <= relativeMax && relativeMax <= maxUseBound
      where
        (relativeMin, relativeMax) = relativeOffsets (Offset.offset (coerce displacement) definition) state.offset
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

selectAlternative :: Bool -> [Part] -> Builder -> State -> Maybe ([Part], State)
selectAlternative _alternative [] _acc _state = error "selectAlternative: unexpected end of input"
selectAlternative alternative (part : parts) acc state =
  case part of
    Rigid builder -> selectAlternative alternative parts (appendPart acc part) $ state {offset = offset (ArrayBuilder.size builder) state.offset}
    Define _ -> error "selectAlternative: unexpected Define"
    Use _ _ -> error "selectAlternative: unexpected Use"
    Flexible parts0 parts1
      | alternative -> rigidize (parts1 <> parts) acc state
      | otherwise -> rigidize (parts0 <> parts) acc state

toArrayBuilder :: Builder -> (ArrayBuilder, HashMap Label (Offset, [(Offset, LabelUse)]))
toArrayBuilder (Builder initialParts) =
  go
    (Foldable.toList initialParts)
    State
      { offset = mempty
      , definitions = mempty
      , rigidUses = mempty
      , flexibleUseCount = 0
      , valid = mempty
      }
  where
    go :: [Part] -> State -> (ArrayBuilder, HashMap Label (Offset, [(Offset, LabelUse)]))
    go [] state = (mempty, HashMap.intersectionWith (\(Offset.Flexible o _) uses -> (o, uses)) state.definitions state.rigidUses)
    go [Rigid builder] state = (builder, HashMap.intersectionWith (\(Offset.Flexible o _) uses -> (o, uses)) state.definitions state.rigidUses)
    go parts state = case rigidize parts mempty $ rewind state of
      Nothing -> error "toBuilder: impossible"
      Just (parts', state')
        | state'.flexibleUseCount > 0
        , state.flexibleUseCount == state'.flexibleUseCount ->
            -- We're not making any progress: see if selecting alternatives in
            -- the first flexible use gets us unstuck.
            case catMaybes [selectAlternative alternative parts' mempty $ rewind state' | alternative <- [False, True]] of
              [] -> error "toBuilder: no alternative works"
              (parts'', state'') : _ -> go parts'' state''
        | otherwise -> go parts' state'

run :: Builder -> ArrayBuilder
run builder =
  ArrayBuilder.overlays $
    instructions : [useBuilder definition uses | (definition, uses) <- HashMap.elems labels]
  where
    (instructions, labels) = toArrayBuilder builder
    useBuilder definition uses =
      ArrayBuilder.overlays
        [ ArrayBuilder.skip (useOffset + use.writeOffset) <> case use.size of
          Int8 -> ArrayBuilder.int8 $ fromIntegral $ definition + coerce use.displacement - useOffset
          Int32 -> ArrayBuilder.int32 $ fromIntegral $ definition + coerce use.displacement - useOffset
        | (useOffset, use) <- uses
        ]

flexible :: Builder -> Builder -> Builder
flexible (Builder l) (Builder r) = Builder $ pure $ Flexible (toList l) (toList r)

word8 :: Word8 -> Builder
word8 = Builder . pure . Rigid . ArrayBuilder.word8

word16 :: Word16 -> Builder
word16 = Builder . pure . Rigid . ArrayBuilder.word16

word32 :: Word32 -> Builder
word32 = Builder . pure . Rigid . ArrayBuilder.word32

word64 :: Word64 -> Builder
word64 = Builder . pure . Rigid . ArrayBuilder.word64

int8 :: Int8 -> Builder
int8 = Builder . pure . Rigid . ArrayBuilder.int8

int16 :: Int16 -> Builder
int16 = Builder . pure . Rigid . ArrayBuilder.int16

int32 :: Int32 -> Builder
int32 = Builder . pure . Rigid . ArrayBuilder.int32

int64 :: Int64 -> Builder
int64 = Builder . pure . Rigid . ArrayBuilder.int64
