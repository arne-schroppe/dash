module Language.Dash.Constants (
  maxRegisters
, maxInteger
, maxSymbols
, tupleSymbolId
, listConsSymbolId
, listEmptySymbolId
, trueSymbolId
, falseSymbolId
) where

maxRegisters, maxInteger, maxSymbols :: Int
maxRegisters = 32
maxInteger = 0x1FFFFF
maxSymbols = maxInteger



tupleSymbolId, listConsSymbolId, listEmptySymbolId, trueSymbolId, falseSymbolId :: String
trueSymbolId = "true"
falseSymbolId = "false"
tupleSymbolId = "tuple"
listConsSymbolId = "list"
listEmptySymbolId = "empty-list"


