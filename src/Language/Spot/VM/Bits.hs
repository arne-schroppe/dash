module Language.Spot.VM.Bits (
  VMValue(..)
, decode
, encodeNumber
, encodePlainSymbol
, encodeCompoundSymbolRef
, encodeCompoundSymbolHeader
, encodeMatchHeader
, decodeMatchHeader
, encodeMatchVar

) where

import           Data.Bits
import           Data.Word
import           Language.Spot.IR.Data
import           Language.Spot.VM.Types


decode :: VMWord -> [Word32] -> SymbolNameList -> VMValue
decode w ctable symNames =
  let tag = getTag w in
  let value = getValue w in
  decode' tag value
  where decode' t v | t==tagNumber     = VMNumber v
                    | t==tagPlainSymbol   = VMSymbol (symNames !! (fromIntegral v)) []
                    | t==tagCompoundSymbol = decodeCompoundSymbol v ctable symNames
                    | otherwise        = error $ "Unknown tag " ++ (show t)


decodeCompoundSymbol :: Integral a => a -> [VMWord] -> SymbolNameList -> VMValue
decodeCompoundSymbol addr ctable symNames =
  let subCTable = drop (fromIntegral addr) ctable in
  let (symId, nArgs) = decodeCompoundSymbolHeader (head subCTable) in
  let decoded = map (\v -> decode v ctable symNames) (take (fromIntegral nArgs) $ tail subCTable) in
  let symName = symNames !! (fromIntegral symId) in
  VMSymbol symName decoded


encodeNumber :: VMWord -> VMWord
encodeNumber = makeVMValue tagNumber . ensureRange

encodePlainSymbol :: VMWord -> VMWord
encodePlainSymbol = makeVMValue tagPlainSymbol . ensureRange

encodeCompoundSymbolRef :: VMWord -> VMWord
encodeCompoundSymbolRef = makeVMValue tagCompoundSymbol . ensureRange

ensureRange :: (Ord a, Num a) => a -> a
ensureRange v = if v < 0 || v > 0x0FFFFFFF then error "Value outside of range" else v

encodeMatchHeader :: VMWord -> VMWord
encodeMatchHeader n = matchData 1 n

decodeMatchHeader :: VMWord -> VMWord
decodeMatchHeader h = h .&. 0x7FFFFFF

encodeMatchVar :: VMWord -> VMWord
encodeMatchVar n = matchData 0 n

matchData :: VMWord -> VMWord -> VMWord
matchData mtag n =
  let cropped = n .&. 0x7FFFFFF in
  let mtagVal = mtag `shiftL` (32 - 5) in
  makeVMValue tagMatchData (cropped .|. mtagVal)

encodeCompoundSymbolHeader :: VMWord -> VMWord -> VMWord
encodeCompoundSymbolHeader symId n = (symId `shiftL` 16) .|. n

decodeCompoundSymbolHeader :: VMWord -> (VMWord, VMWord)
decodeCompoundSymbolHeader v = ((v .&. 0xFFFF0000) `rotateL` 16, v .&. 0x0000FFFF)


makeVMValue :: VMWord -> VMWord -> VMWord
makeVMValue tag i = i .|. (tag `shiftL` (32 - 4))


getTag, getValue :: (Bits a, Num a) => a -> a
getTag v = (v .&. 0xF0000000) `rotateL` 4
getValue v = v .&. 0x0FFFFFFF


tagNumber, tagPlainSymbol, tagCompoundSymbol, tagMatchData :: VMWord
tagNumber = 0x0
tagPlainSymbol = 0x4
tagCompoundSymbol = 0x5
tagMatchData = 0xF


