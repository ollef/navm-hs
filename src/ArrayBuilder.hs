{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoFieldSelectors #-}

module ArrayBuilder where

import Control.Monad
import Control.Monad.Primitive (unsafeIOToPrim, unsafePrimToIO)
import Control.Monad.ST
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as ByteString
import Data.Char
import Data.Foldable
import Data.Int
import Data.Primitive.ByteArray
import Data.Primitive.Ptr
import Data.Semigroup
import Data.Word
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (castPtr)
import GHC.Exts hiding (build)
import GHC.ST
import Offset (Offset (Offset))
import qualified System.ByteOrder as ByteOrder
import Text.Printf (printf)
import Prelude

data ArrayBuilder = ArrayBuilder
  { size :: !Offset
  , function :: !(forall s. (# State# s, Addr# #) -> State# s)
  }

instance Show ArrayBuilder where
  showsPrec _ ab =
    foldrByteArray (\(c :: Word8) -> (showString (printf "%02x " c) .)) id (run ab)

newtype ArrayBuilderM a = ArrayBuilderM (State ArrayBuilder a)
  deriving (Functor, Applicative, Monad, MonadFix)

exec :: ArrayBuilderM a -> ArrayBuilder
exec (ArrayBuilderM s) = execState s mempty

offset :: ArrayBuilderM Offset
offset = ArrayBuilderM $ gets size

emit :: ArrayBuilder -> ArrayBuilderM ()
emit ab = ArrayBuilderM $ modify (<> ab)

size :: ArrayBuilder -> Offset
size ab = ab.size

function :: ArrayBuilder -> ((# State# s, Addr# #) -> State# s)
function (ArrayBuilder _ f) = f

st :: Offset -> (forall s. Ptr Word8 -> ST s ()) -> ArrayBuilder
st bytes f =
  ArrayBuilder
    bytes
    \(# s, addr #) -> do
      let (ST inner) = f (Ptr addr)
      let !(# s', () #) = inner s
      s'

run :: ArrayBuilder -> ByteArray
run builder =
  runST do
    arr <- newPinnedByteArray $ fromIntegral $ size builder
    let !(Ptr startAddr) = mutableByteArrayContents arr
    ST
      ( \s -> do
          let !s' = function builder (# s, startAddr #)
          (# s', () #)
      )
    unsafeFreezeByteArray arr

instance Semigroup ArrayBuilder where
  ab1 <> ab2 =
    ArrayBuilder
      (size ab1 + size ab2)
      ( \x@(# _, addr #) -> do
          let !s = function ab1 x
              !(Ptr addr') = advancePtr (Ptr addr :: Ptr Word8) (coerce $ size ab1)
          function ab2 (# s, addr' #)
      )

instance Monoid ArrayBuilder where
  mempty = skip 0
  mconcat = foldl' (<>) mempty

skip :: Offset -> ArrayBuilder
skip bytes = ArrayBuilder (coerce bytes) \(# s, _addr #) -> s

zeros :: Offset -> ArrayBuilder
zeros bytes = st bytes \ptr -> setPtr ptr (fromIntegral bytes) 0

overlay :: ArrayBuilder -> ArrayBuilder -> ArrayBuilder
overlay ab1 ab2 =
  ArrayBuilder
    (max (size ab1) (size ab2))
    \(# s, addr #) -> do
      let !s' = function ab1 (# s, addr #)
      function ab2 (# s', addr #)

overlays :: [ArrayBuilder] -> ArrayBuilder
overlays builders =
  ArrayBuilder
    (foldl' (\n (ArrayBuilder size_ _) -> max n size_) 0 builders)
    (go builders)
  where
    go [] (# s, _ #) = s
    go (ArrayBuilder _ function_ : builders') arg@(# _, addr #) = do
      let !s' = function_ arg
      go builders' (# s', addr #)

char8 :: Char -> ArrayBuilder
char8 = word8 . fromIntegral . ord

word8 :: Word8 -> ArrayBuilder
word8 w = st 1 \ptr -> writeOffPtr ptr 0 w

word16 :: Word16 -> ArrayBuilder
word16 w = st 2 \ptr -> writeOffPtr (castPtr ptr) 0 $ ByteOrder.toLittleEndian w

word32 :: Word32 -> ArrayBuilder
word32 w = st 4 \ptr -> writeOffPtr (castPtr ptr) 0 $ ByteOrder.toLittleEndian w

word64 :: Word64 -> ArrayBuilder
word64 w = st 8 \ptr -> writeOffPtr (castPtr ptr) 0 $ ByteOrder.toLittleEndian w

int8 :: Int8 -> ArrayBuilder
int8 = word8 . fromIntegral

int16 :: Int16 -> ArrayBuilder
int16 = word16 . fromIntegral

int32 :: Int32 -> ArrayBuilder
int32 = word32 . fromIntegral

int64 :: Int64 -> ArrayBuilder
int64 = word64 . fromIntegral

byteArray :: ByteArray -> ArrayBuilder
byteArray arr =
  st (fromIntegral $ sizeofByteArray arr) \ptr ->
    copyByteArrayToPtr ptr arr 0 (sizeofByteArray arr)

byteString :: ByteString -> ArrayBuilder
byteString (ByteString.BS foreignPtr sz) =
  st (fromIntegral sz) \dst ->
    unsafeIOToPrim $
      withForeignPtr foreignPtr \src ->
        copyPtr dst src sz

toByteString :: ArrayBuilder -> ByteString
toByteString ab =
  runST $
    unsafeIOToPrim $
      ByteString.create (fromIntegral $ size ab) \(Ptr addr) ->
        unsafePrimToIO $ ST \s -> (# function ab (# s, addr #), () #)
