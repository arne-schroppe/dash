module Language.Dash.Constants (
  maxRegisters
, minInteger
, maxInteger
, maxSymbols
, tupleSymbolId
, listConsSymbolId
, listEmptySymbolId
, trueSymbolId
, falseSymbolId
, numberBias
) where

maxRegisters, minInteger, maxInteger, maxSymbols, numberBias :: Int
maxRegisters = 32
maxSymbols = maxInteger

maxInteger = 0xFFFFF
minInteger = (-0xFFFFF)
numberBias = maxInteger



-- TODO rename these to *SymbolName
tupleSymbolId, listConsSymbolId, listEmptySymbolId, trueSymbolId, falseSymbolId :: String
trueSymbolId = "true"
falseSymbolId = "false"
tupleSymbolId = "tuple"
listConsSymbolId = "list"
listEmptySymbolId = "empty-list"


