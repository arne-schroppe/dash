module Language.Dash.VM.DataEncoding (
  VMValue(..)
, decode
, encodeNumber
, encodePlainSymbol
, encodeCompoundSymbolRef
, encodeDynamicCompoundSymbolRef
, encodeCompoundSymbolHeader
, encodeMatchHeader
, decodeMatchHeader
, encodeMatchVar
) where

import           Data.Bits
import           Data.Word
import           Language.Dash.Constants
import           Language.Dash.IR.Data
import           Language.Dash.VM.Types
import           Language.Dash.VM.VM     (getVMHeapArray, getVMHeapValue)


decode :: VMWord -> [Word32] -> SymbolNameList -> IO VMValue
decode w ctable symNames =
  let tag = getTag w in
  let value = getValue w in
  decode' tag value
  where decode' t v | t==tagNumber                = return $ VMNumber ((fromIntegral v) - numberBias)
                    | t==tagPlainSymbol           = return $ VMSymbol (symNames !! fromIntegral v) []
                    | t==tagCompoundSymbol        = decodeCompoundSymbol v ctable symNames
                    | t==tagDynamicCompoundSymbol = decodeDynCompoundSymbol v ctable symNames
                    | t==tagClosure               = return VMClosure
                    | t==tagFunction              = return VMFunction
                    | otherwise                   = error $ "Unknown tag " ++ show t


decodeCompoundSymbol :: VMWord -> [VMWord] -> SymbolNameList -> IO VMValue
decodeCompoundSymbol addr ctable symNames = do
  let subCTable = drop (fromIntegral addr) ctable
  let (symId, nArgs) = decodeCompoundSymbolHeader (head subCTable)
  decoded <- mapM (\v -> decode v ctable symNames)
                  (take (fromIntegral nArgs) $ tail subCTable)
  let symName = symNames !! symIdToInt symId
  return $ VMSymbol symName decoded

decodeDynCompoundSymbol :: VMWord -> [VMWord] -> SymbolNameList -> IO VMValue
decodeDynCompoundSymbol addr ctable symNames = do
  symHeader <- getVMHeapValue addr
  let (symId, count) = decodeCompoundSymbolHeader symHeader
  let symName = symNames !! symIdToInt symId
  values <- getVMHeapArray (addr + compoundSymbolHeaderLength) count
  decoded <- mapM (\v -> decode v ctable symNames) values
  return $ VMSymbol symName decoded

encodeNumber :: Int -> VMWord
encodeNumber = makeVMValue tagNumber . ensureRange . bias . fromIntegral

encodePlainSymbol :: SymId -> VMWord
encodePlainSymbol = makeVMValue tagPlainSymbol . ensureRange . fromIntegral . symIdToInt

encodeCompoundSymbolRef :: ConstAddr -> VMWord
encodeCompoundSymbolRef = makeVMValue tagCompoundSymbol
                          . ensureRange
                          . fromIntegral
                          . constAddrToInt

encodeDynamicCompoundSymbolRef :: HeapAddr -> VMWord
encodeDynamicCompoundSymbolRef = makeVMValue tagDynamicCompoundSymbol
                                 . ensureRange
                                 . fromIntegral
                                 . heapAddrToInt

bias :: Int -> VMWord
bias n = fromIntegral $ n + numberBias

ensureRange :: (Ord a, Num a, Show a) => a -> a
-- ensureRange v = if v < (fromIntegral 0) || v > (fromIntegral $ maxInteger + numberBias) then error ("Value outside of range: " ++ (show v)) else v
ensureRange v = if v > (fromIntegral $ maxInteger + numberBias) then error ("Value outside of range: " ++ (show v)) else v

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


tagNumber, tagPlainSymbol, tagCompoundSymbol, tagMatchData, tagFunction, tagDynamicCompoundSymbol, tagClosure, tagString :: VMWord
tagNumber = 0x0
tagPlainSymbol = 0x4
tagCompoundSymbol = 0x5
tagClosure = 0x6
tagFunction = 0x7
tagDynamicCompoundSymbol = 0x8
tagString = 0x9
tagMatchData = 0xF

compoundSymbolHeaderLength :: VMWord
compoundSymbolHeaderLength = 1

