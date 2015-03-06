module Language.Spot.VM.Types where

import Data.Word

data VMValue =
    VMNumber Word32 -- Number
  | VMSymbol String [VMValue] -- symbolId
  deriving (Show, Eq)
