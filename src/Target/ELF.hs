{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Target.ELF where

import ArrayBuilder (ArrayBuilder)
import qualified ArrayBuilder
import Data.Bits
import Data.ByteString (ByteString)
import Data.Foldable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.List as List
import Data.Maybe
import Data.Word
import Offset (Offset)

file :: ArrayBuilder -> ArrayBuilder
file code =
  let sectionHeaders_ =
        sectionHeaders
          SectionHeadersParameters
            { codeSection =
                Location
                  { offset = fromIntegral $ ArrayBuilder.size r2
                  , size = fromIntegral $ ArrayBuilder.size code
                  }
            , sectionNameSection =
                Location
                  { offset = fromIntegral $ ArrayBuilder.size r4
                  , size = fromIntegral $ ArrayBuilder.size sectionNames
                  }
            , sectionNameIndices
            }
      (sectionNames, sectionNameIndices) = stringTable $ fst <$> sectionHeaders_
      programHeaders_ =
        programHeaders
          ProgramHeadersParameters
            { codeSegment =
                Location
                  { offset = fromIntegral $ ArrayBuilder.size r2
                  , size = fromIntegral $ ArrayBuilder.size code
                  }
            }
      r1 =
        elfHeader
          ElfHeaderParameters
            { programHeader =
                HeaderInfo
                  { offset = fromIntegral $ ArrayBuilder.size r1
                  , entrySize = maybe 0 (fromIntegral . ArrayBuilder.size) $ listToMaybe programHeaders_
                  , entries = fromIntegral $ length programHeaders_
                  }
            , sectionHeader =
                HeaderInfo
                  { offset = fromIntegral $ ArrayBuilder.size r5
                  , entrySize = maybe 0 (fromIntegral . ArrayBuilder.size . snd) $ listToMaybe sectionHeaders_
                  , entries = fromIntegral $ length sectionHeaders_
                  }
            , sectionNameSectionEntryIndex = maybe (error "no section name section entry index") fromIntegral $ List.findIndex ((== ".shstrtab") . fst) sectionHeaders_
            }
      r2 = padToMultipleOf (fromIntegral pageSize) $ r1 <> mconcat programHeaders_
      r3 = r2 <> code
      r4 = r3 <> data_
      r5 = r4 <> sectionNames
      r6 = r5 <> foldMap snd sectionHeaders_
   in r6

padToMultipleOf :: Offset -> ArrayBuilder -> ArrayBuilder
padToMultipleOf x b = b <> ArrayBuilder.zeros padding
  where
    size = ArrayBuilder.size b
    desiredSize = ((size + x - 1) `div` x) * x
    padding = desiredSize - size

data HeaderInfo = HeaderInfo
  { offset :: !Word64
  , entrySize :: !Word16
  , entries :: !Word16
  }
  deriving (Show)

data ElfHeaderParameters = ElfHeaderParameters
  { programHeader :: !HeaderInfo
  , sectionHeader :: !HeaderInfo
  , sectionNameSectionEntryIndex :: !Word16
  }
  deriving (Show)

elfHeader :: ElfHeaderParameters -> ArrayBuilder
elfHeader ElfHeaderParameters {..} = result
  where
    result =
      mconcat
        [ ident
        , type_
        , machine
        , version
        , ArrayBuilder.word64 entrypointAddress
        , ArrayBuilder.word64 programHeader.offset
        , ArrayBuilder.word64 sectionHeader.offset
        , flags
        , elfHeaderSize
        , ArrayBuilder.word16 programHeader.entrySize
        , ArrayBuilder.word16 programHeader.entries
        , ArrayBuilder.word16 sectionHeader.entrySize
        , ArrayBuilder.word16 sectionHeader.entries
        , ArrayBuilder.word16 sectionNameSectionEntryIndex
        ]
    type_ = ArrayBuilder.word16 0x03 -- DYN (Position-Independent Executable file)
    machine = ArrayBuilder.word16 0x3e -- AMD x86-64
    version = ArrayBuilder.word32 1
    flags = ArrayBuilder.word32 0
    elfHeaderSize = ArrayBuilder.word16 $ fromIntegral $ ArrayBuilder.size result

entrypointAddress :: Word64
entrypointAddress = 0x1000

ident :: ArrayBuilder
ident =
  mconcat
    [ magicNumber
    , class_
    , dataEncoding
    , identVersion
    , abi
    , abiVersion
    , ArrayBuilder.skip 7 -- padding
    ]
  where
    magicNumber = ArrayBuilder.word8 0x7f <> ArrayBuilder.byteString "ELF"
    class_ = ArrayBuilder.word8 2 -- 64 bit
    dataEncoding = ArrayBuilder.word8 1 -- Little-endian
    identVersion = ArrayBuilder.word8 1
    abi = ArrayBuilder.word8 0x00 -- UNIX - System V
    abiVersion = ArrayBuilder.word8 0

stringTable :: [ByteString] -> (ArrayBuilder, HashMap ByteString Offset)
stringTable strings = foldl' go mempty ("" : strings)
  where
    go (table, indices) string =
      HashMap.alterF
        ( \maybeIndex -> case maybeIndex of
            Nothing ->
              ( table <> ArrayBuilder.byteString string <> ArrayBuilder.word8 0
              , Just $ ArrayBuilder.size table
              )
            Just _ -> (table, maybeIndex)
        )
        string
        indices

data Location = Location
  { offset :: !Word64
  , size :: !Word64
  }

newtype ProgramHeadersParameters = ProgramHeadersParameters
  { codeSegment :: Location
  }

programHeaders :: ProgramHeadersParameters -> [ArrayBuilder]
programHeaders ProgramHeadersParameters {..} =
  [ codeSegmentProgramHeader
  ]
  where
    codeSegmentProgramHeader =
      mconcat
        [ ArrayBuilder.word32 0x1 -- load
        , ArrayBuilder.word32 (0x1 .|. 0x4) -- flags: execute, read
        , ArrayBuilder.word64 codeSegment.offset
        , -- offset
          ArrayBuilder.word64 entrypointAddress -- virtual address
        , ArrayBuilder.word64 0 -- physical address
        , ArrayBuilder.word64 codeSegment.size
        , -- file size
          ArrayBuilder.word64 codeSegment.size
        , -- memory size
          ArrayBuilder.word64 pageSize -- alignment
        ]

pageSize :: Word64
pageSize = 0x1000

data_ :: ArrayBuilder
data_ = mempty

data SectionHeadersParameters = SectionHeadersParameters
  { codeSection :: !Location
  , sectionNameSection :: !Location
  , sectionNameIndices :: HashMap ByteString Offset
  }

sectionHeaders :: SectionHeadersParameters -> [(ByteString, ArrayBuilder)]
sectionHeaders SectionHeadersParameters {..} =
  [ ("", nullSectionHeader)
  , (".text", codeSectionHeader)
  , (".shstrtab", sectionNameSectionHeader)
  ]
  where
    nullSectionHeader = ArrayBuilder.zeros 64
    codeSectionHeader =
      mconcat
        [ ArrayBuilder.word32 $ fromIntegral $ sectionNameIndices HashMap.! ".text"
        , ArrayBuilder.word32 0x1 -- Progbits
        , ArrayBuilder.word64 $ alloc .|. execute
        , ArrayBuilder.word64 entrypointAddress -- virtual address
        , ArrayBuilder.word64 codeSection.offset
        , -- offset
          ArrayBuilder.word64 codeSection.size
        , -- size
          ArrayBuilder.word32 0 -- link
        , ArrayBuilder.word32 0 -- info
        , ArrayBuilder.word64 4 -- addralign
        , ArrayBuilder.word64 0 -- entry size
        ]
    sectionNameSectionHeader =
      mconcat
        [ ArrayBuilder.word32 $ fromIntegral $ sectionNameIndices HashMap.! ".shstrtab"
        , ArrayBuilder.word32 0x3 -- string table
        , ArrayBuilder.word64 0 -- flags
        , ArrayBuilder.word64 0 -- virtual address
        , ArrayBuilder.word64 sectionNameSection.offset
        , -- offset
          ArrayBuilder.word64 sectionNameSection.size
        , -- size
          ArrayBuilder.word32 0 -- link
        , ArrayBuilder.word32 0 -- info
        , ArrayBuilder.word64 4 -- addralign
        , ArrayBuilder.word64 0 -- entry size
        ]
    alloc = 0x2
    execute = 0x2
