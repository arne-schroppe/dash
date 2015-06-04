module Language.Spot.VM.Assembler (
  assemble
, assembleWithEncodedConstTable
, atomizeConstTable
, AtomicConstant(..)
) where

-- TODO put this into its own namespace

-- Translates [[Tac]] to data for the virtual machine

import           Control.Monad.State
import           Data.Bits
import qualified Data.IntMap            as IntMap
import           Data.List
import           Data.Maybe
import           Data.Word
import           Language.Spot.IR.Data
import           Language.Spot.IR.Tac
import           Language.Spot.VM.Bits
import           Language.Spot.VM.Types

import           Debug.Trace

-- TODO do we encode nested symbols depth-first or breadth-first? Try both and measure performance!


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


assembleTac :: [VMWord] -> (Int -> VMWord) -> Tac -> Word32
assembleTac funcAddrs addrConv opc =
  let r = fromIntegral in
  let i = fromIntegral in
  case opc of
    Tac_ret r0          -> instructionRI   0 (r r0) 0
    Tac_load_i r0 i     -> instructionRI   1 (r r0) i
    Tac_load_addr r0 a  -> instructionRI   1 (r r0) (addrConv a)
    Tac_load_f r0 fi    -> instructionRI   1 (r r0) (funcAddrs !! fi)
    Tac_load_ps r0 s    -> instructionRI   2 (r r0) (i s)
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
    Tac_set_arg arg r1 n  -> instructionRRR 13 (i arg) (r r1) (i n)
    Tac_tail_call fr n    -> instructionRRR 14 (i 0) (r fr) (i n)
    Tac_tail_call_cl fr n -> instructionRRR 15 (i 0) (r fr) (i n)
    Tac_set_cl_arg clr r1 n -> instructionRRR 16 (r clr) (r r1) (i n)



instBits = 32
opcBits = 6
regBits = 5


instructionRI opcId register value =
  (opcId `shiftL` (instBits - opcBits)) .|. (register `shiftL` (instBits - (opcBits + regBits))) .|. value

instructionRRR opcId r0 r1 r2 =
   (opcId `shiftL` (instBits - opcBits))
  .|. (r0 `shiftL` (instBits - (opcBits + regBits)))
  .|. (r1 `shiftL` (instBits - (opcBits + 2 * regBits)))
  .|. (r2 `shiftL` (instBits - (opcBits + 3 * regBits)))




------ CONST ENCODING -----------


-- TODO put into separate module
-- TODO explain the algorithm (here and elsewhere with complicated algorithms)


data AtomicConstant =
    ACPlainSymbol SymId
  | ACCompoundSymbolRef ConstAddr
  | ACCompoundSymbolHeader SymId Int
  | ACNumber Int
  | ACMatchHeader Int
  | ACMatchVar Int
  deriving (Show, Eq)

encodeConstant c = case c of
  ACPlainSymbol sid -> encodePlainSymbol $ fromIntegral sid
  ACCompoundSymbolRef addr -> encodeCompoundSymbolRef $ fromIntegral addr
  ACCompoundSymbolHeader sid n -> encodeCompoundSymbolHeader (fromIntegral sid) (fromIntegral n)
  ACNumber n -> encodeNumber $ fromIntegral n
  ACMatchHeader n -> encodeMatchHeader $ fromIntegral n
  ACMatchVar n -> encodeMatchVar $ fromIntegral n


data ConstAtomizationState = ConstAtomizationState {
  constants         :: [Constant]
, workQueue         :: [Constant]
, addrMap           :: IntMap.IntMap VMWord
, atomized          :: [AtomicConstant] -- should be a Sequence
, reservedSpace     :: Int
, numAtomizedConsts :: Int
}

encodeConstTable :: ConstTable -> ([VMWord], Int -> VMWord)
encodeConstTable ctable =
  let (atoms, mapping) = atomizeConstTable ctable in
  (map encodeConstant atoms, (mapping IntMap.!) )


atomizeConstTable :: ConstTable -> ([AtomicConstant], IntMap.IntMap VMWord)
atomizeConstTable ctable =
  let state = execState (encTable ctable) initState in
  (atomized state, addrMap state)
  where
    initState = ConstAtomizationState {
                    constants = ctable
                  , workQueue = []
                  , addrMap = IntMap.fromList []
                  , atomized = []
                  , reservedSpace = 0
                  , numAtomizedConsts = 0
                }
    encTable ctable = whileJust atomizeConst popWorkItem


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
          numAtomized <- gets numAtomizedConsts
          let currentAddr = length $ atomized state
          addAddrMapping numAtomized $ fromIntegral currentAddr
          state' <- get
          put $ state' { numAtomizedConsts = numAtomized + 1, constants = tail cs }
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
  let used = length $ atomized state
  let reserved = reservedSpace state
  let pendingItems = workQueue state
  let pending = foldl (\acc c -> acc + (spaceNeededByConstant c)) 0 pendingItems
  return $ used + reserved + pending

spaceNeededByConstant c = case c of
  CNumber _ -> 1
  CPlainSymbol _ -> 1
  CMatchVar _ -> 1
  CCompoundSymbol _ args -> 1 + length args
  CMatchData args -> 1 + length args

addAtomized atoms = do
  state <- get
  put $ state { atomized = (atomized state) ++ atoms }

setReservedSpace n = do
  state <- get
  put $ state { reservedSpace = n }




atomizeConst c = case c of
  CNumber n -> addAtomized [ACNumber n]
  CPlainSymbol sid -> addAtomized [ACPlainSymbol sid]
  CCompoundSymbol sid args -> atomizeCompoundSymbol sid args
  CMatchData args -> atomizeMatchData args
  x -> error $ "Unable to encode top-level constant " ++ show x


atomizeCompoundSymbol sid args = do
  setReservedSpace (1 + length args)
  let symbolHeader = ACCompoundSymbolHeader (fromIntegral sid) (fromIntegral $ length args)
  atomizedArgs <- mapM atomizeConstArg args
  addAtomized $ symbolHeader : atomizedArgs
  setReservedSpace 0

atomizeMatchData args = do
  setReservedSpace (1 + length args)
  let matchHeader = ACMatchHeader (fromIntegral $ length args)
  atomizedArgs <- mapM atomizeConstArg args
  addAtomized $ matchHeader : atomizedArgs
  setReservedSpace $ 0


atomizeConstArg c = case c of
  CNumber n -> return $ ACNumber n
  CPlainSymbol sid -> return $ ACPlainSymbol sid
  CMatchVar n -> return $ ACMatchVar n
  ds@(CCompoundSymbol _ _) -> do
                addr <- nextFreeAddress
                pushWorkItem ds
                return $ ACCompoundSymbolRef addr
  x -> error $ "Unable to encode constant as argument: " ++ show x


