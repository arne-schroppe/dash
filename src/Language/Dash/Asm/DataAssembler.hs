module Language.Dash.Asm.DataAssembler (
  encodeConstTable

-- These two are only exposed for debugging  TODO move them to another module?
, atomizeConstTable
, AtomicConstant(..)
) where

import           Control.Monad.State           hiding (state)
import           Data.List.Split
import qualified Data.Map                      as Map
import qualified Data.Sequence                 as Seq
import           Language.Dash.IR.Data
import qualified Language.Dash.VM.DataEncoding as Enc
import           Language.Dash.VM.Types

{-

Data Assembler
~~~~~~~~~~~~~~

Encodes data of type Constant for the virtual machine.


TODO describe the runtime representation of all data
TODO rename const table to constant pool?

-}



-- TODO explain the algorithm or simplify it (the latter is probably better)


type ConstAddressMap = ConstAddr -> VMWord
type ConstAtomizationState a = State ConstAtomizationEnv a


-- The ConstAddressMap is a conversion function from the virtual constant
-- addresses used in the Opcode ir to the real binary offsets for the vm
encodeConstTable :: ConstTable -> Seq.Seq VMWord -> ([VMWord], ConstAddressMap)
encodeConstTable ctable funcMap =
  let (atoms, mapping) = atomizeConstTable ctable funcMap in
  (map encodeConstant atoms, (mapping Map.!) )


-- We receive data as an array of Data.Constant objects. The first step is
-- to split this representation into their atomic parts. This is what this
-- function does. The next step encodes those atomic parts into their
-- byte representation for the vm.
atomizeConstTable :: ConstTable -> Seq.Seq VMWord -> ([AtomicConstant], Map.Map ConstAddr VMWord)
atomizeConstTable ctable funcMap =
  let state = execState encTable (emptyConstAtomizationEnv ctable funcMap) in
  (atomized state, addrMap state)
  where
    encTable = whileJust atomizeConstant popWorkItem


whileJust :: (b -> ConstAtomizationState a)
          -> ConstAtomizationState (Maybe b)
          -> ConstAtomizationState ()
whileJust f source = do
  next <- source
  case next of
    Nothing -> return ()
    Just x -> do
      _ <- f x
      whileJust f source


atomizeConstant :: Constant -> ConstAtomizationState ()
atomizeConstant c = case c of
  CNumber n                  -> addAtomized [ACNumber n]
  CPlainSymbol sid           -> addAtomized [ACPlainSymbol sid]
  CCompoundSymbol sid args   -> atomizeCompoundSymbol sid args
  CMatchData args            -> atomizeMatchData args
  CString str                -> atomizeString str
  COpaqueSymbol sid own args -> atomizeOpaqueSymbol sid own args
  CFunction addr             -> atomizeFunction addr
  CCompoundSymbolRef caddr   -> addAtomized [ACCompoundSymbolRef caddr]
  x -> error $ "Unable to encode top-level constant " ++ show x

atomizeFunction :: FuncAddr -> ConstAtomizationState ()
atomizeFunction addr = do
  faddr <- actualFuncAddr addr
  addAtomized [ACFunction faddr]

actualFuncAddr :: FuncAddr -> ConstAtomizationState Int
actualFuncAddr addr = do
  funcMap <- gets functionMap
  return $ fromIntegral $ funcMap `Seq.index` funcAddrToInt addr

atomizeCompoundSymbol :: SymId -> [Constant] -> ConstAtomizationState ()
atomizeCompoundSymbol sid args = do
  setReservedSpace (1 + length args) -- TODO this is duplicated logic (see below)
  let symbolHeader = ACCompoundSymbolHeader sid (fromIntegral $ length args)
  atomizedArgs <- mapM atomizeConstArg args
  addAtomized $ symbolHeader : atomizedArgs
  setReservedSpace 0

atomizeOpaqueSymbol :: SymId -> SymId -> [Constant] -> ConstAtomizationState ()
atomizeOpaqueSymbol sid owner args = do
  setReservedSpace (2 + length args)
  let symbolHeader = ACOpaqueSymbolHeader sid (fromIntegral $ length args)
  atomizedArgs <- mapM atomizeConstArg args
  addAtomized $ symbolHeader : (ACPlainSymbol owner) : atomizedArgs
  setReservedSpace 0

atomizeMatchData :: [Constant] -> ConstAtomizationState ()
atomizeMatchData args = do
  setReservedSpace (1 + length args)
  let matchHeader = ACMatchHeader (fromIntegral $ length args)
  atomizedArgs <- mapM atomizeConstArg args
  addAtomized $ matchHeader : atomizedArgs
  setReservedSpace 0


atomizeString :: String -> ConstAtomizationState ()
atomizeString str = do
  let numChunks = numStringChunksForString str
  let nullString = str ++ "\0"
  -- fill rest of string with zeroes
  let adjustedString = nullString ++
        replicate (bytesPerVMWord - (length nullString `rem` bytesPerVMWord)) '\0'
  let chunks = chunksOf bytesPerVMWord adjustedString
  let encChunks = map (\ [c1, c2, c3, c4] -> ACStringChunk c1 c2 c3 c4) chunks
  let header = ACStringHeader (length str) numChunks
  addAtomized $ header : encChunks


bytesPerVMWord :: Int
bytesPerVMWord = 4

numStringChunksForString :: String -> Int
numStringChunksForString str =
  let len = length str + 1 in -- add 1 for terminating \0
  let (numChunks, remainder) = len `divMod` bytesPerVMWord in
  let adjust = if remainder /= 0 then 1 else 0 in
  numChunks + adjust


atomizeConstArg :: Constant -> ConstAtomizationState AtomicConstant
atomizeConstArg c = case c of
  CNumber n        -> return $ ACNumber n
  CPlainSymbol sid -> return $ ACPlainSymbol sid
  CMatchVar n      -> return $ ACMatchVar n
  ds@(CCompoundSymbol _ _) -> do
                addr <- nextFreeAddress
                pushWorkItem ds
                return $ ACCompoundSymbolRef addr
  CFunction addr    -> actualFuncAddr addr >>= return . ACFunction
  x -> error $ "Unable to encode constant as argument: " ++ show x


-- State

data ConstAtomizationEnv = ConstAtomizationEnv {
  constants         :: [Constant]
, workQueue         :: [Constant]
, addrMap           :: Map.Map ConstAddr VMWord
, atomized          :: [AtomicConstant] -- should be a Sequence
, reservedSpace     :: Int
, numAtomizedConsts :: Int
, functionMap       :: Seq.Seq VMWord
}


emptyConstAtomizationEnv :: [Constant] -> Seq.Seq VMWord -> ConstAtomizationEnv
emptyConstAtomizationEnv ctable funcMap = ConstAtomizationEnv {
  constants         = ctable
, workQueue         = []
, addrMap           = Map.empty
, atomized          = []
, reservedSpace     = 0
, numAtomizedConsts = 0
, functionMap       = funcMap
}


-- While encoding for example a compound symbol, the elements of that symbol
-- are pushed onto the workQueue and atomized next. If the workQueue is empty
-- we continue with the next constant from our original input. When we're
-- done we return Nothing.
-- TODO I'm quite sure that this can be written much more elegantly
popWorkItem :: ConstAtomizationState (Maybe Constant)
popWorkItem = do
  state <- get
  case (workQueue state, constants state) of
    ([], []) ->
        return Nothing
    ([], cs) -> do
        numAtomized <- gets numAtomizedConsts
        let currentAddr = fromIntegral $ length $ atomized state
        addAddrMapping (mkConstAddr numAtomized) currentAddr
        state' <- get
        put $ state' { numAtomizedConsts = numAtomized + 1, constants = tail cs }
        return $ Just $ head cs
    (ws, _) -> do
        put (state { workQueue = tail ws })
        return $ Just $ head ws


addAddrMapping :: ConstAddr -> VMWord -> ConstAtomizationState ()
addAddrMapping src dest = do
  state <- get
  let newMap = Map.insert src dest (addrMap state)
  put $ state { addrMap = newMap }


pushWorkItem :: Constant -> ConstAtomizationState ()
pushWorkItem c = do
  state <- get
  let workQ = workQueue state
  put $ state { workQueue = workQ ++ [c] }


nextFreeAddress :: ConstAtomizationState ConstAddr
nextFreeAddress = do
  state <- get
  let used = length $ atomized state
  let reserved = reservedSpace state
  let pendingItems = workQueue state
  let pending = foldl (\acc c -> acc + spaceNeededByConstant c) 0 pendingItems
  return $ mkConstAddr $ used + reserved + pending


spaceNeededByConstant :: Constant -> Int
spaceNeededByConstant c = case c of
  CNumber _              -> 1
  CPlainSymbol _         -> 1
  CMatchVar _            -> 1
  CCompoundSymbol _ args -> 1 + length args
  COpaqueSymbol _ _ args -> 2 + length args
  CMatchData args        -> 1 + length args
  CString str            -> 1 + numStringChunksForString str
  CFunction _            -> 1
  CCompoundSymbolRef _   -> 1


addAtomized :: [AtomicConstant] -> ConstAtomizationState ()
addAtomized atoms = do
  state <- get
  put $ state { atomized = atomized state ++ atoms }


setReservedSpace :: Int -> ConstAtomizationState ()
setReservedSpace n = do
  state <- get
  put $ state { reservedSpace = n }



-- Byte encoding for data

data AtomicConstant =
    ACPlainSymbol SymId
  | ACCompoundSymbolRef ConstAddr
  | ACCompoundSymbolHeader SymId Int
  | ACOpaqueSymbolHeader SymId Int
  | ACNumber Int
  | ACMatchHeader Int
  | ACMatchVar Int
  | ACStringHeader Int Int     -- string length, num chunks
  | ACStringChunk Char Char Char Char -- with ascii chars and VMWord as Word32 this would be 4 chars per string chunk
  | ACFunction Int
  deriving (Show, Eq)


encodeConstant :: AtomicConstant -> VMWord
encodeConstant c = case c of
  ACPlainSymbol sid            -> Enc.encodePlainSymbol sid
  ACCompoundSymbolRef addr     -> Enc.encodeCompoundSymbolRef addr
  ACCompoundSymbolHeader sid n -> Enc.encodeCompoundSymbolHeader sid n
  ACNumber n                   -> Enc.encodeNumber n
  ACMatchHeader n              -> Enc.encodeMatchHeader n
  ACMatchVar n                 -> Enc.encodeMatchVar n
  ACStringHeader len numChunks -> Enc.encodeStringHeader len numChunks
  ACStringChunk b1 b2 b3 b4    -> Enc.encodeStringChunk b1 b2 b3 b4
  ACOpaqueSymbolHeader sid n   -> Enc.encodeOpaqueSymbolHeader sid n
  ACFunction addr              -> Enc.encodeFunctionRef addr

