{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Label where

import Data.ByteString
import Data.Hashable
import Data.String

newtype Label = Label ByteString
  deriving (Eq, Show, Hashable, IsString)
