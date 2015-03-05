module Language.Spot.VM.Types where

import Data.Word

data VMValue =
    VMNumber Word32 -- Number
  | VMSymbol Word32 -- symbolId
  | VMDataSymbol Word32 -- address
  deriving (Show, Eq)
