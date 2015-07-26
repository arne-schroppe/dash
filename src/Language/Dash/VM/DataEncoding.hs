module Language.Dash.VM.DataEncoding (
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
import           Language.Dash.IR.Data
import           Language.Dash.VM.Types


decode :: VMWord -> [Word32] -> SymbolNameList -> VMValue
decode w ctable symNames =
  let tag = getTag w in
  let value = getValue w in
  decode' tag value
  where decode' t v | t==tagNumber     = VMNumber v
                    | t==tagPlainSymbol   = VMSymbol (symNames !! fromIntegral v) []
                    | t==tagCompoundSymbol = decodeCompoundSymbol v ctable symNames
                    | t==tagClosure = VMClosure
                    | t==tagFunction = VMFunction
                    | otherwise        = error $ "Unknown tag " ++ show t


decodeCompoundSymbol :: VMWord -> [VMWord] -> SymbolNameList -> VMValue
decodeCompoundSymbol addr ctable symNames =
  let subCTable = drop (fromIntegral addr) ctable in
  let (symId, nArgs) = decodeCompoundSymbolHeader (head subCTable) in
  let decoded = map (\v -> decode v ctable symNames) (take (fromIntegral nArgs) $ tail subCTable) in
  let symName = symNames !! symIdToInt symId in
  VMSymbol symName decoded


encodeNumber :: Int -> VMWord
encodeNumber = makeVMValue tagNumber . ensureRange . fromIntegral

encodePlainSymbol :: SymId -> VMWord
encodePlainSymbol = makeVMValue tagPlainSymbol . ensureRange . fromIntegral . symIdToInt

encodeCompoundSymbolRef :: ConstAddr -> VMWord
encodeCompoundSymbolRef = makeVMValue tagCompoundSymbol . ensureRange . fromIntegral . constAddrToInt

ensureRange :: (Ord a, Num a) => a -> a
ensureRange v = if v < 0 || v > 0x0FFFFFFF then error "Value outside of range" else v

data MatchDataType = MatchHeader | MatchVar

matchDataSubTag :: MatchDataType -> VMWord
matchDataSubTag MatchHeader = 1
matchDataSubTag MatchVar    = 0

encodeMatchHeader :: Int -> VMWord
encodeMatchHeader = matchData MatchHeader

decodeMatchHeader :: VMWord -> Int
decodeMatchHeader h = fromIntegral $ h .&. 0x7FFFFFF

encodeMatchVar :: Int -> VMWord
encodeMatchVar = matchData MatchVar

matchData :: MatchDataType -> Int -> VMWord
matchData mtype n =
  let mtag = matchDataSubTag mtype in
  let cropped = fromIntegral $ n .&. 0x7FFFFFF in
  let mtagVal = mtag `shiftL` (32 - 5) in
  makeVMValue tagMatchData (cropped .|. mtagVal)


encodeCompoundSymbolHeader :: SymId -> Int -> VMWord
encodeCompoundSymbolHeader symId n = fromIntegral $ (symIdToInt symId `shiftL` 16) .|. n

decodeCompoundSymbolHeader :: VMWord -> (SymId, Int)
decodeCompoundSymbolHeader v = (mkSymId $ fromIntegral $ (v .&. 0xFFFF0000) `rotateL` 16,
                                fromIntegral $ v .&. 0x0000FFFF)


makeVMValue :: VMWord -> VMWord -> VMWord
makeVMValue tag i = i .|. (tag `shiftL` (32 - 4))



getTag, getValue :: (Bits a, Num a) => a -> a
getTag v = (v .&. 0xF0000000) `rotateL` 4
getValue v = v .&. 0x0FFFFFFF


tagNumber, tagPlainSymbol, tagCompoundSymbol, tagMatchData, tagFunction, tagClosure :: VMWord
tagNumber = 0x0
tagPlainSymbol = 0x4
tagCompoundSymbol = 0x5
tagClosure = 0x6
tagFunction = 0x7
tagMatchData = 0xF


