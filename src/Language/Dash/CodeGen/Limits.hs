module Language.Dash.CodeGen.Limits (
  maxRegisters
, maxInteger
, maxSymbols
) where

maxRegisters, maxInteger, maxSymbols :: Int
maxRegisters = 32
maxInteger = 0x1FFFFF
maxSymbols = maxInteger
