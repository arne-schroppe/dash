module Language.Spot.VM.Types where

import Data.Word

type VMWord = Word32

data VMValue =
    VMNumber VMWord -- Number
  | VMSymbol String [VMValue] -- symbolId
  deriving (Show, Eq)
