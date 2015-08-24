module Language.Dash.Limits (
  maxRegisters
, minInteger
, maxInteger
, maxSymbols
, intBias
) where

maxRegisters, minInteger, maxInteger, maxSymbols, intBias :: Int
maxRegisters = 32
maxSymbols = maxInteger

maxInteger = 0xFFFFF
minInteger = (-0xFFFFF)
intBias = maxInteger


