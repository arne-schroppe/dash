module Language.Spot.VM.Bits (
  VMValue(..)
, encNumber
, encSymbol
, encDataSymbolRef
, decode
, encMatchHeader
, encMatchVar
, encDataSymbolHeader

) where

import Data.Word
import Data.Bits

import Language.Spot.VM.Types
import Language.Spot.IR.Tac



encNumber :: VMWord -> VMWord
encNumber = makeVMValue tagNumber . ensureRange

encSymbol :: VMWord -> VMWord
encSymbol = makeVMValue tagSymbol . ensureRange

encDataSymbolRef :: VMWord -> VMWord
encDataSymbolRef = makeVMValue tagDataSymbol . ensureRange

ensureRange v = if v < 0 || v > 0x0FFFFFFF then error "Value outside of range" else v



decode :: VMWord -> [Word32] -> SymbolNameList -> VMValue
decode w ctable symNames =
  let tag = getTag w in
  let value = getValue w in
  decode' tag value
  where decode' t v | t==tagNumber     = VMNumber v
                    | t==tagSymbol     = VMSymbol (symNames !! (fromIntegral v)) []
                    | t==tagDataSymbol = decodeDataSymbol v ctable symNames
                    | otherwise        = error $ "Unknown tag " ++ (show t)

decodeDataSymbol addr ctable symNames =
  let subCTable = drop (fromIntegral addr) ctable in
  let (symId, nArgs) = decDataSymbolHeader (head subCTable) in
  let decoded = map (\v -> decode v ctable symNames) (take (fromIntegral nArgs) $ tail subCTable) in
  let symName = symNames !! (fromIntegral symId) in
  VMSymbol symName decoded


encMatchHeader :: VMWord -> VMWord
encMatchHeader n = matchData 1 n

encMatchVar :: VMWord -> VMWord
encMatchVar n = matchData 0 n

matchData mtag n =
  let cropped = n .&. 0x7FFFFFF in
  let mtagVal = mtag `shiftL` (32 - 5) in
  makeVMValue tagMatchData (cropped .|. mtagVal)

encDataSymbolHeader :: VMWord -> VMWord -> VMWord
encDataSymbolHeader symId n = (symId `shiftL` 16) .|. n

decDataSymbolHeader :: VMWord -> (VMWord, VMWord)
decDataSymbolHeader v = ((v .&. 0xFFFF0000) `rotateL` 16, v .&. 0x0000FFFF)


makeVMValue :: VMWord -> VMWord -> VMWord
makeVMValue tag i = i .|. (tag `shiftL` (32 - 4))


getTag v = (v .&. 0xF0000000) `rotateL` 4
getValue v = v .&. 0x0FFFFFFF


tagNumber = 0x0 :: VMWord
tagSymbol = 0x4 :: VMWord
tagDataSymbol = 0x5 :: VMWord
tagMatchData = 0xF :: VMWord


