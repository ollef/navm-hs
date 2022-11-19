{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NoFieldSelectors #-}

module Target.ELF where

import ArrayBuilder (ArrayBuilder, ArrayBuilderM)
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
  ArrayBuilder.exec $ mdo
    let sectionHeaders_ =
          sectionHeaders
            SectionHeadersParameters
              { codeSection =
                  Location
                    { offset = fromIntegral codeSegmentOffset
                    , size = fromIntegral $ ArrayBuilder.size code
                    }
              , sectionNameSection =
                  Location
                    { offset = fromIntegral sectionNameSectionOffset
                    , size = fromIntegral $ ArrayBuilder.size sectionNames
                    }
              , sectionNameIndices
              }
    let (sectionNames, sectionNameIndices) = stringTable $ fst <$> sectionHeaders_
    let programHeaders_ =
          programHeaders
            ProgramHeadersParameters
              { codeSegment =
                  Location
                    { offset = fromIntegral codeSegmentOffset
                    , size = fromIntegral $ ArrayBuilder.size code
                    }
              }
    ArrayBuilder.emit $
      elfHeader
        ElfHeaderParameters
          { programHeader =
              HeaderInfo
                { offset = fromIntegral programHeaderOffset
                , entrySize = maybe 0 (fromIntegral . ArrayBuilder.size) $ listToMaybe programHeaders_
                , entries = fromIntegral $ length programHeaders_
                }
          , sectionHeader =
              HeaderInfo
                { offset = fromIntegral sectionHeaderOffset
                , entrySize = maybe 0 (fromIntegral . ArrayBuilder.size . snd) $ listToMaybe sectionHeaders_
                , entries = fromIntegral $ length sectionHeaders_
                }
          , sectionNameSectionEntryIndex =
              maybe (error "no section name section entry index") fromIntegral $
                List.findIndex ((== ".shstrtab") . fst) sectionHeaders_
          }
    programHeaderOffset <- ArrayBuilder.offset
    mapM_ ArrayBuilder.emit programHeaders_
    padToMultipleOf $ fromIntegral pageSize
    codeSegmentOffset <- ArrayBuilder.offset
    ArrayBuilder.emit code
    sectionNameSectionOffset <- ArrayBuilder.offset
    ArrayBuilder.emit sectionNames
    sectionHeaderOffset <- ArrayBuilder.offset
    mapM_ (ArrayBuilder.emit . snd) sectionHeaders_

padToMultipleOf :: Offset -> ArrayBuilderM ()
padToMultipleOf x = do
  offset <- ArrayBuilder.offset
  let desiredOffset = ((offset + x - 1) `div` x) * x
      padding = desiredOffset - offset
  ArrayBuilder.emit $ ArrayBuilder.zeros padding

data HeaderInfo = HeaderInfo
  { offset :: Word64
  , entrySize :: Word16
  , entries :: Word16
  }
  deriving (Show)

data ElfHeaderParameters = ElfHeaderParameters
  { programHeader :: HeaderInfo
  , sectionHeader :: HeaderInfo
  , sectionNameSectionEntryIndex :: Word16
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
  { offset :: Word64
  , size :: Word64
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
  { codeSection :: Location
  , sectionNameSection :: Location
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
    execute = 0x4
