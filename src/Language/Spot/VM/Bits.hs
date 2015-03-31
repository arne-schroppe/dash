module Language.Spot.VM.Bits (
  VMValue(..)
, encNumber
, encAtomicSymbol
, encCompoundSymbolRef
, decode
, encMatchHeader
, decodeMatchHeader
, encMatchVar
, encCompoundSymbolHeader

) where

import Data.Word
import Data.Bits

import Language.Spot.VM.Types
import Language.Spot.IR.Tac



encNumber :: VMWord -> VMWord
encNumber = makeVMValue tagNumber . ensureRange

encAtomicSymbol :: VMWord -> VMWord
encAtomicSymbol = makeVMValue tagSymbol . ensureRange

encCompoundSymbolRef :: VMWord -> VMWord
encCompoundSymbolRef = makeVMValue tagDataSymbol . ensureRange

ensureRange v = if v < 0 || v > 0x0FFFFFFF then error "Value outside of range" else v



decode :: VMWord -> [Word32] -> SymbolNameList -> VMValue
decode w ctable symNames =
  let tag = getTag w in
  let value = getValue w in
  decode' tag value
  where decode' t v | t==tagNumber     = VMNumber v
                    | t==tagSymbol     = VMSymbol (symNames !! (fromIntegral v)) []
                    | t==tagDataSymbol = decodeCompoundSymbol v ctable symNames
                    | otherwise        = error $ "Unknown tag " ++ (show t)

decodeCompoundSymbol addr ctable symNames =
  let subCTable = drop (fromIntegral addr) ctable in
  let (symId, nArgs) = decCompoundSymbolHeader (head subCTable) in
  let decoded = map (\v -> decode v ctable symNames) (take (fromIntegral nArgs) $ tail subCTable) in
  let symName = symNames !! (fromIntegral symId) in
  VMSymbol symName decoded


encMatchHeader :: VMWord -> VMWord
encMatchHeader n = matchData 1 n

decodeMatchHeader :: VMWord -> VMWord
decodeMatchHeader h = h .&. 0x7FFFFFF

encMatchVar :: VMWord -> VMWord
encMatchVar n = matchData 0 n

matchData mtag n =
  let cropped = n .&. 0x7FFFFFF in
  let mtagVal = mtag `shiftL` (32 - 5) in
  makeVMValue tagMatchData (cropped .|. mtagVal)

encCompoundSymbolHeader :: VMWord -> VMWord -> VMWord
encCompoundSymbolHeader symId n = (symId `shiftL` 16) .|. n

decCompoundSymbolHeader :: VMWord -> (VMWord, VMWord)
decCompoundSymbolHeader v = ((v .&. 0xFFFF0000) `rotateL` 16, v .&. 0x0000FFFF)


makeVMValue :: VMWord -> VMWord -> VMWord
makeVMValue tag i = i .|. (tag `shiftL` (32 - 4))


getTag v = (v .&. 0xF0000000) `rotateL` 4
getValue v = v .&. 0x0FFFFFFF


tagNumber = 0x0 :: VMWord
tagSymbol = 0x4 :: VMWord
tagDataSymbol = 0x5 :: VMWord
tagMatchData = 0xF :: VMWord


