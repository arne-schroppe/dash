module Language.Dash.Constants (
  maxRegisters
, minInteger
, maxInteger
, maxSymbols
, numberBias
) where

maxRegisters, minInteger, maxInteger, maxSymbols, numberBias :: Int
maxRegisters = 32
maxSymbols = maxInteger

maxInteger = 0xFFFFF
minInteger = (-0xFFFFF)
numberBias = maxInteger




