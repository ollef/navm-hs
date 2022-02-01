{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Label where

import Data.ByteString
import Data.Hashable

newtype Label = Label ByteString
  deriving (Eq, Show, Hashable)
