module Language.Dash.VM.Types where

import Data.Word

type VMWord = Word32

data VMValue =
    VMNumber VMWord
  | VMSymbol String [VMValue]
  | VMClosure -- TODO add meaningful data 
  | VMFunction -- TODO add meaningful data (name, arguments, etc)
  deriving (Show, Eq)
