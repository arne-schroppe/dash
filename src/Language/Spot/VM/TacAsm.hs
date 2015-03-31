module Language.Spot.VM.TacAsm (
  assemble
, assembleWithEncodedConstTable
, encodeConstTableToAtomicConsts
, AtomicConstant(..)
) where

-- Translates [[Tac]] to data for the virtual machine

import Control.Monad.State
import Data.Word
import Data.List
import Data.Bits
import Data.Maybe
import Language.Spot.IR.Tac
import Language.Spot.VM.Types
import Language.Spot.VM.Bits
import qualified Data.IntMap as IntMap

import Debug.Trace

-- TODO rename this module simply to Assembler

-- TODO do we encode nested symbols depth-first or breadth-first? Try both and measure performance!




-- TODO instead of fromIntegral use something to convert constTable addresses
assemble :: [[Tac]] -> ConstTable -> SymbolNameList -> ([VMWord], [VMWord], SymbolNameList)
assemble funcs ctable symnames = 
  let (consts, addrConvert) = encodeConstTable ctable in
  assembleWithEncodedConstTable funcs consts addrConvert symnames


assembleWithEncodedConstTable :: [[Tac]] -> [VMWord] -> (Int -> VMWord) -> SymbolNameList -> ([VMWord], [VMWord], SymbolNameList)
assembleWithEncodedConstTable funcs encCTable cAddrConverter symnames =
  (map (assembleTac funcAddrs cAddrConverter) instructions, encCTable, symnames)
  where instructions = fst combined
        funcAddrs = snd combined
        combined = combineFunctions funcs


combineFunctions :: [[Tac]] -> ([Tac], [VMWord])
combineFunctions funcs = (fst combined, reverse $ snd combined)
  where combined = foldl calcFuncAddr ([], []) funcs
        calcFuncAddr acc funcInstrs =
          let allInstrs = fst acc in
          let funcAddrs = snd acc in
          ( allInstrs ++ funcInstrs, (fromIntegral $ length allInstrs) : funcAddrs )


-- TODO convert constant addresse
assembleTac :: [VMWord] -> (Int -> VMWord) -> Tac -> Word32
assembleTac funcAddrs addrConv opc =
  let r = fromIntegral in
  let i = fromIntegral in
  case opc of
    Tac_ret             -> instructionRI   0 0 0
    Tac_load_i r0 i     -> instructionRI   1 (r r0) i
    Tac_load_addr r0 a  -> instructionRI   1 (r r0) (addrConv a)
    Tac_load_f r0 fi    -> instructionRI   1 (r r0) (funcAddrs !! fi)
    Tac_load_as r0 s    -> instructionRI   2 (r r0) (i s)
    Tac_load_cs r0 a    -> instructionRI   3 (r r0) (addrConv a)
    Tac_load_c r0 a     -> instructionRI   4 (r r0) (addrConv a)
    Tac_add r0 r1 r2    -> instructionRRR  5 (r r0) (r r1) (r r2)
    Tac_sub r0 r1 r2    -> instructionRRR  6 (r r0) (r r1) (r r2)
    Tac_move r0 r1      -> instructionRRR  7 (r r0) (r r1) (i 0)
    Tac_call r0 fr n    -> instructionRRR  8 (r r0) (r fr) (i n)
    Tac_call_cl r0 fr n -> instructionRRR  9 (r r0) (r fr) (i n)
    Tac_make_cl r0 fr n -> instructionRRR 10 (r r0) (r fr) (i n)
    Tac_jmp n           -> instructionRI  11 0 (i n)
    Tac_match r0 r1 r2  -> instructionRRR 12 (r r0) (r r1) (r r2)



instSize = 32

instructionRI opcId register value =
  (opcId `shiftL` (instSize - 4)) .|. (register `shiftL` (instSize - (4 + 5))) .|. value

instructionRRR opcId r0 r1 r2 =
   (opcId `shiftL` (instSize - 4))
  .|. (r0 `shiftL` (instSize - (4 + 5)))
  .|. (r1 `shiftL` (instSize - (4 + 2 * 5)))
  .|. (r2 `shiftL` (instSize - (4 + 3 * 5)))







------ CONST ENCODING -----------


-- TODO explain the algorithm (here and elsewhere with complicated algorithms)


data AtomicConstant = 
    ACAtomicSymbol SymId
  | ACCompoundSymbolRef ConstAddr
  | ACCompoundSymbolHeader SymId Int
  | ACNumber Int
  | ACMatchHeader Int
  | ACMatchVar Int
  deriving (Show, Eq)

encodeConstantToBits c = case c of
  ACAtomicSymbol sid -> encAtomicSymbol $ fromIntegral sid
  ACCompoundSymbolRef addr -> encCompoundSymbolRef $ fromIntegral addr
  ACCompoundSymbolHeader sid n -> encCompoundSymbolHeader (fromIntegral sid) (fromIntegral n)
  ACNumber n -> encNumber $ fromIntegral n
  ACMatchHeader n -> encMatchHeader $ fromIntegral n
  ACMatchVar n -> encMatchVar $ fromIntegral n


data ConstEncodingState = ConstEncodingState {
  constants :: [Constant]
, workQueue :: [Constant]
, addrMap :: IntMap.IntMap VMWord
, encoded :: [AtomicConstant] -- should be a Sequence
, reservedSpace :: Int
, numEncodedConsts :: Int
}

encodeConstTable :: ConstTable -> ([VMWord], Int -> VMWord)
encodeConstTable ctable =
  let (bitcs, mapping) = encodeConstTableToAtomicConsts ctable in
  (map encodeConstantToBits bitcs, (mapping IntMap.!) )


encodeConstTableToAtomicConsts :: ConstTable -> ([AtomicConstant], IntMap.IntMap VMWord)
encodeConstTableToAtomicConsts ctable =
  let state = execState (encTable ctable) initState in
  (encoded state, addrMap state)
  where
    initState = ConstEncodingState { 
                    constants = ctable
                  , workQueue = []
                  , addrMap = IntMap.fromList []
                  , encoded = []
                  , reservedSpace = 0
                  , numEncodedConsts = 0
                }
    encTable ctable = whileJust encodeConst popWorkItem


whileJust f source = do
  next <- source
  case next of
    Nothing -> return ()
    Just x -> do
      f x
      whileJust f source

popWorkItem = do
  state <- get
  case (workQueue state, constants state) of
    ([], []) -> return Nothing
    ([], cs) -> do
          numEncoded <- gets numEncodedConsts
          let currentAddr = length $ encoded state
          addAddrMapping numEncoded $ fromIntegral currentAddr
          state' <- get
          put $ state' { numEncodedConsts = numEncoded + 1, constants = tail cs }
          return $ Just $ head cs
    (ws, _) -> do
          put (state { workQueue = tail ws })
          return $ Just $ head ws

addAddrMapping src dest = do
  state <- get
  let newMap = IntMap.insert src dest (addrMap state)
  put $ state { addrMap = newMap }

pushWorkItem c = do
  state <- get
  let workQ = workQueue state
  put $ state { workQueue = workQ ++ [c] }

nextFreeAddress = do
  state <- get
  let used = length $ encoded state
  let reserved = reservedSpace state
  let pendingItems = workQueue state
  let pending = foldl (\acc c -> acc + (spaceNeededByConstant c)) 0 pendingItems
  return $ used + reserved + pending

spaceNeededByConstant c = case c of
  CNumber _ -> 1
  CAtomicSymbol _ -> 1
  CMatchVar _ -> 1
  CCompoundSymbol _ args -> 1 + length args
  CMatchData args -> 1 + length args

addEncoded enc = do
  state <- get
  put $ state { encoded = (encoded state) ++ enc }

setReservedSpace n = do
  state <- get
  put $ state { reservedSpace = n }




encodeConst c = case c of
  CNumber n -> addEncoded [ACNumber n]
  CAtomicSymbol sid -> addEncoded [ACAtomicSymbol sid]
  CCompoundSymbol sid args -> encodeCompoundSymbol sid args
  CMatchData args -> encodeMatchData args
  x -> error $ "Unable to encode top-level constant " ++ show x


encodeCompoundSymbol sid args = do
  setReservedSpace (1 + length args)
  let symbolHeader = ACCompoundSymbolHeader (fromIntegral sid) (fromIntegral $ length args)
  encodedArgs <- mapM encodeConstArg args
  addEncoded $ symbolHeader : encodedArgs
  setReservedSpace 0

encodeMatchData args = do
  setReservedSpace (1 + length args)
  let matchHeader = ACMatchHeader (fromIntegral $ length args)
  encodedArgs <- mapM encodeConstArg args
  addEncoded $ matchHeader : encodedArgs
  setReservedSpace $ 0


encodeConstArg c = case c of
  CNumber n -> return $ ACNumber n
  CAtomicSymbol sid -> return $ ACAtomicSymbol sid
  CMatchVar n -> return $ ACMatchVar n
  ds@(CCompoundSymbol _ _) -> do
                addr <- nextFreeAddress
                pushWorkItem ds
                return $ ACCompoundSymbolRef addr
  x -> error $ "Unable to encode constant as argument: " ++ show x


