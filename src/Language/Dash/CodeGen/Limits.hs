module Language.Dash.CodeGen.Limits (
  maxRegisters
, maxInteger
, maxSymbols
) where


maxRegisters, maxInteger, maxSymbols :: Int

maxRegisters = 32
maxSymbols = 1024 -- TODO this number is completely arbitrary, find out what the real limit is
maxInteger = 0x1FFFFF
