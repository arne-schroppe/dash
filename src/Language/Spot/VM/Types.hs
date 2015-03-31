module Language.Spot.VM.Types where

import Data.Word

type VMWord = Word32

data VMValue =
    VMNumber VMWord
  | VMSymbol String [VMValue]
  deriving (Show, Eq)
