module Language.Spot.VM.Bits (
  VMValue(..)
, encNumber
, encSymbol
, encDataSymbol
, decode
, encMatchHeader
, encMatchVar
, encDataSymbolHeader

) where

import Data.Word
import Data.Bits

import Language.Spot.VM.Types
import Language.Spot.IR.Opcode


encNumber :: Word32 -> Word32
encNumber = makeVMValue tagNumber . ensureRange

encSymbol :: Word32 -> Word32
encSymbol = makeVMValue tagSymbol . ensureRange

encDataSymbol :: Word32 -> Word32
encDataSymbol = makeVMValue tagDataSymbol . ensureRange

ensureRange v = if v < 0 || v > 0x0FFFFFFF then error "Value outside of range" else v



decode :: Word32 -> SymbolNameList -> VMValue
decode w symNames =
  let tag = getTag w in
  let value = getValue w in
  decode' tag value
  where decode' t v | t==tagNumber = VMNumber v
                    | t==tagSymbol = VMSymbol (symNames !! (fromIntegral v)) [] -- TODO use symbol-name-table to determine name
                 -- | t==tagDataSymbol = VMSymbol v [] -- TODO the value in a data symbol is an address, also add arguments
                    | otherwise    = error $ "Unknown tag " ++ (show t)

encMatchHeader :: Word32 -> Word32
encMatchHeader n = matchData 1 n

encMatchVar :: Word32 -> Word32
encMatchVar n = matchData 0 n

matchData mtag n =
  let cropped = n .&. 0x7FFFFFF in
  let mtagVal = mtag `shiftL` (32 - 5) in
  makeVMValue tagMatchData (cropped .|. mtagVal)

encDataSymbolHeader :: Word32 -> Word32 -> Word32
encDataSymbolHeader symId n = (symId `shiftL` 16) .|. n


makeVMValue :: Word32 -> Word32 -> Word32
makeVMValue tag i = i .|. (tag `shiftL` (32 - 4))


getTag v = (v .&. 0xF0000000) `rotateL` 4
getValue v = v .&. 0x0FFFFFFF


tagNumber = 0x0 :: Word32
tagSymbol = 0x4 :: Word32
tagDataSymbol = 0x5 :: Word32
tagMatchData = 0xF :: Word32


