{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Dash.IR.Data (
  Constant (..)
, ConstTable
, SymbolNameList
, Reg
, mkReg
, regToInt
, SymId
, mkSymId
, symIdToInt
, FuncAddr
, mkFuncAddr
, funcAddrToInt
, ConstAddr
, mkConstAddr
, constAddrToInt
, HeapAddr
, mkHeapAddr
, heapAddrToInt
, Name
) where

import           Control.Exception
import           Language.Dash.Limits

-- Intermediate representation for data static runtime data

data Constant =
    CPlainSymbol SymId
  | CCompoundSymbol SymId [Constant]
  | COpaqueSymbol SymId SymId [Constant] -- own sym id, owner, fields
  | CString String
  | CNumber Int
  | CMatchData [Constant]
  | CMatchVar Int -- Can only be used inside CMatchData
  | CFunction FuncAddr
  | CCompoundSymbolRef ConstAddr
  deriving (Show, Eq)


type ConstTable = [Constant] -- TODO move these out of here
type SymbolNameList = [String]


type Name = String


newtype Reg =
  MkReg Int
  deriving (Show, Eq, Ord, Num)

mkReg :: Int -> Reg
mkReg i =
  assert (i >= 0 && i < maxRegisters) $
  MkReg i

regToInt :: Reg -> Int
regToInt (MkReg i) = i


newtype SymId = MkSymId Int
  deriving (Show, Eq, Ord)

mkSymId :: Int -> SymId
mkSymId i =
  assert (i >= 0 && i < maxSymbols) $
  MkSymId i

symIdToInt :: SymId -> Int
symIdToInt (MkSymId i) = i


newtype FuncAddr = MkFuncAddr Int
  deriving (Show, Eq, Ord)

mkFuncAddr :: Int -> FuncAddr
mkFuncAddr i =
  assert (i >= 0) $
  MkFuncAddr i

funcAddrToInt :: FuncAddr -> Int
funcAddrToInt (MkFuncAddr i) = i


-- TODO is ConstAddr before or after encoding the data? not clear. We should have separate types!
newtype ConstAddr = MkConstAddr Int
  deriving (Eq, Show, Ord)

mkConstAddr :: Int -> ConstAddr
mkConstAddr i =
  assert (i >= 0) $
  MkConstAddr i

constAddrToInt :: ConstAddr -> Int
constAddrToInt (MkConstAddr i) = i


newtype HeapAddr = MkHeapAddr Int
  deriving (Eq, Show, Ord)

mkHeapAddr :: Int -> HeapAddr
mkHeapAddr i =
  assert (i >= 0) $
  MkHeapAddr i

heapAddrToInt :: HeapAddr -> Int
heapAddrToInt (MkHeapAddr i) = i

