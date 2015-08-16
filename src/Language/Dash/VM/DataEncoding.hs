module Language.Dash.VM.DataEncoding (
  VMValue(..)
, decode
, encodeNumber
, encodePlainSymbol
, encodeCompoundSymbolRef
, encodeDynamicCompoundSymbolRef
, encodeCompoundSymbolHeader
, encodeOpaqueSymbolHeader
, encodeMatchHeader
, decodeMatchHeader
, encodeMatchVar
, encodeStringRef
, encodeStringHeader
, encodeStringChunk
, encodeFunctionRef
) where

import           Data.Bits
import           Data.Word
import           Foreign.C.String        (castCharToCChar, castCCharToChar)
import           Language.Dash.Constants
import           Language.Dash.IR.Data
import           Language.Dash.VM.Types
import           Language.Dash.VM.VM     (getVMHeapArray, getVMHeapValue)
import Numeric


decode :: VMWord -> [Word32] -> SymbolNameList -> IO VMValue
decode w ctable symNames =
  let tag = getTag w in
  let value = getValue w in
  decode' tag value
  where decode' t v | t==tagNumber                = return $ VMNumber ((fromIntegral v) - intBias)
                    | t==tagPlainSymbol           = return $ VMSymbol (symNames !! fromIntegral v) []
                    | t==tagCompoundSymbol        = decodeCompoundSymbol v ctable symNames
                    | t==tagDynamicCompoundSymbol = decodeDynamicCompoundSymbol v ctable symNames
                    | t==tagClosure               = return VMClosure
                    | t==tagFunction              = return VMFunction
                    | t==tagString                = decodeConstantString v ctable
                    | t==tagDynamicString         = decodeDynamicString v
                    | otherwise                   = error $ "Unknown tag " ++ show t


-- Number

encodeNumber :: Int -> VMWord
encodeNumber = makeVMValue tagNumber . ensureNumberRange . bias . fromIntegral

bias :: Int -> VMWord
bias n = fromIntegral $ n + intBias


-- Function

encodeFunctionRef :: Int -> VMWord
encodeFunctionRef = makeVMValue tagFunction . fromIntegral

-- Symbols

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

encodeCompoundSymbolHeader :: SymId -> Int -> VMWord
encodeCompoundSymbolHeader symId n =
  makeVMValue tagCompoundSymbol $ fromIntegral $ (symIdToInt symId `shiftL` 14) .|. n

encodeOpaqueSymbolHeader :: SymId -> Int -> VMWord
encodeOpaqueSymbolHeader symId n =
  makeVMValue tagOpaqueSymbol $ fromIntegral $ (symIdToInt symId `shiftL` 14) .|. n

decodeCompoundSymbolHeader :: VMWord -> (SymId, Int)
decodeCompoundSymbolHeader v =
  let tag = getTag v in
  if tag /= tagCompoundSymbol then
    error "Expected string tag in string header"
  else
    let value = getValue v in
    (mkSymId $ fromIntegral $ (value .&. high14Bits) `rotateR` 14,
     fromIntegral $ value .&. low14Bits)


decodeCompoundSymbol :: VMWord -> [VMWord] -> SymbolNameList -> IO VMValue
decodeCompoundSymbol addr ctable symNames = do
  let subCTable = drop (fromIntegral addr) ctable
  let (symId, nArgs) = decodeCompoundSymbolHeader (head subCTable)
  decoded <- mapM (\v -> decode v ctable symNames)
                  (take (fromIntegral nArgs) $ tail subCTable)
  let symName = symNames !! symIdToInt symId
  return $ VMSymbol symName decoded


decodeDynamicCompoundSymbol :: VMWord -> [VMWord] -> SymbolNameList -> IO VMValue
decodeDynamicCompoundSymbol addr ctable symNames = do
  symHeader <- getVMHeapValue addr
  let (symId, count) = decodeCompoundSymbolHeader symHeader
  let symName = symNames !! symIdToInt symId
  values <- getVMHeapArray (addr + compoundSymbolHeaderLength) count
  decoded <- mapM (\v -> decode v ctable symNames) values
  return $ VMSymbol symName decoded



-- Strings

decodeConstantString :: VMWord -> [VMWord] -> IO VMValue
decodeConstantString addr ctable = do
  let subCTable = drop (fromIntegral addr) ctable
  let (_, numChunks) = decodeStringHeader (head subCTable)
  let decodedChunks = map decodeStringChunk (take numChunks $ tail subCTable)
  let str = concat decodedChunks
  return $ VMString str

decodeDynamicString :: VMWord -> IO VMValue
decodeDynamicString addr = do
  stringHeader <- getVMHeapValue addr
  let (_, numChunks) = decodeStringHeader stringHeader
  stringBody <- getVMHeapArray (addr + stringHeaderLength) numChunks
  let decodedChunks = map decodeStringChunk stringBody
  let str = concat decodedChunks
  return $ VMString str


encodeStringRef :: ConstAddr -> VMWord
encodeStringRef = makeVMValue tagString
                  . ensureRange
                  . fromIntegral
                  . constAddrToInt


encodeStringHeader :: Int -> Int -> VMWord
encodeStringHeader len numChunks =
  makeVMValue tagString $
              fromIntegral $ (len `shiftL` 14) .|. numChunks


decodeStringHeader :: VMWord -> (Int, Int)
decodeStringHeader v =
  let tag = getTag v in
  if tag /= tagString then
    error "Expected string tag in string header"
  else
    let value = getValue v in
    (fromIntegral $ (value .&. high14Bits) `rotateR` 14,
                        fromIntegral $ value .&. low14Bits)


encodeStringChunk :: Char -> Char -> Char -> Char -> VMWord
encodeStringChunk c1 c2 c3 c4 =
  (fromIntegral $ castCharToCChar c4) `shiftL` (3 * 8)
  .|. (fromIntegral $ castCharToCChar c3) `shiftL` (2 * 8)
  .|. (fromIntegral $ castCharToCChar c2) `shiftL` (1 * 8)
  .|. (fromIntegral $ castCharToCChar c1)


decodeStringChunk :: VMWord -> String
decodeStringChunk encoded =
  let c4 = castCCharToChar $ fromIntegral $ (encoded .&. 0xFF000000) `rotateR` (3 * 8) in
  let c3 = castCCharToChar $ fromIntegral $ (encoded .&. 0x00FF0000) `rotateR` (2 * 8) in
  let c2 = castCCharToChar $ fromIntegral $ (encoded .&. 0x0000FF00) `rotateR` (1 * 8) in
  let c1 = castCCharToChar $ fromIntegral $ (encoded .&. 0x000000FF) in
  let str = [c1, c2, c3, c4] in
  filter (/= '\0') str



-- Match patterns

data MatchDataType = MatchHeader | MatchVar

matchDataSubTag :: MatchDataType -> VMWord
matchDataSubTag MatchHeader = 1
matchDataSubTag MatchVar    = 0

encodeMatchHeader :: Int -> VMWord
encodeMatchHeader = matchData MatchHeader

decodeMatchHeader :: VMWord -> Int
decodeMatchHeader h = fromIntegral $ h .&. low27Bits

encodeMatchVar :: Int -> VMWord
encodeMatchVar = matchData MatchVar

matchData :: MatchDataType -> Int -> VMWord
matchData mtype n =
  let mtag = matchDataSubTag mtype in
  let cropped = fromIntegral $ (fromIntegral n) .&. low27Bits in
  let mtagVal = mtag `shiftL` (32 - 5) in
  makeVMValue tagMatchData (cropped .|. mtagVal)


-- Tools

-- TODO check that data actually fits within the data mask!
makeVMValue :: VMWord -> VMWord -> VMWord
makeVMValue tag i = (i .&. low28Bits) .|. (tag `shiftL` (32 - 4))

getTag, getValue :: (Bits a, Num a) => a -> a
getTag v = (v .&. 0xF0000000) `rotateL` 4
getValue v = v .&. fromIntegral low28Bits


-- TODO check max number
ensureRange :: (Ord a, Num a, Show a) => a -> a
ensureRange v = if v < (fromIntegral 0) || v > (fromIntegral 0xFFFFF) then error ("Value outside of range: " ++ (show v)) else v

ensureNumberRange :: (Ord a, Num a, Show a) => a -> a
ensureNumberRange v = if v > (fromIntegral $ maxInteger + intBias) then error ("Value outside of range: " ++ (show v)) else v




-- Constants, limits


low28Bits, low27Bits, low14Bits, high14Bits :: VMWord
low28Bits = 0x0FFFFFFF
low27Bits = 0x07FFFFFF
low14Bits = 0x3FFF
high14Bits = 0xFFFC000

tagNumber, tagPlainSymbol, tagCompoundSymbol, tagMatchData, tagFunction, tagDynamicCompoundSymbol, tagClosure, tagString, tagDynamicString, tagOpaqueSymbol :: VMWord
tagNumber = 0x0
tagPlainSymbol = 0x4
tagCompoundSymbol = 0x5
tagClosure = 0x6
tagFunction = 0x7
tagDynamicCompoundSymbol = 0x8
tagString = 0x9
tagDynamicString = 0xA
tagOpaqueSymbol = 0xA
tagMatchData = 0xF

compoundSymbolHeaderLength, stringHeaderLength :: VMWord
compoundSymbolHeaderLength = 1
stringHeaderLength = 1



