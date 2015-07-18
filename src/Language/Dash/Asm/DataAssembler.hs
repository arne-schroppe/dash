module Language.Dash.Asm.DataAssembler (
  encodeConstTable

-- These two are only exposed for debugging  TODO move them to another module?
, atomizeConstTable
, AtomicConstant(..)
) where

import           Control.Monad.State    hiding (state)
import qualified Data.IntMap            as IntMap
import           Language.Dash.IR.Data
import qualified Language.Dash.VM.DataEncoding  as Enc
import           Language.Dash.VM.Types

{-

Data Assembler
~~~~~~~~~~~~~~

Encodes data of type Constant for the virtual machine.


TODO describe the runtime representation of all data
TODO rename const table to constant pool?

-}



-- TODO explain the algorithm or simplify it (the latter is probably better)


type ConstAddressMap = Int -> VMWord
type ConstAtomizationState a = State ConstAtomizationEnv a


-- The ConstAddressMap is a conversion function from the virtual constant
-- addresses used in the Tac ir to the real binary offsets for the vm
encodeConstTable :: ConstTable -> ([VMWord], ConstAddressMap)
encodeConstTable ctable =
  let (atoms, mapping) = atomizeConstTable ctable in
  (map encodeConstant atoms, (mapping IntMap.!) )


-- We receive data as an array of Data.Constant objects. The first step is
-- to split this representation into their atomic parts. This is what this
-- function does. The next step encodes those atomic parts into their
-- byte representation for the vm.
atomizeConstTable :: ConstTable -> ([AtomicConstant], IntMap.IntMap VMWord)
atomizeConstTable ctable =
  let state = execState encTable (emptyConstAtomizationEnv ctable) in
  (atomized state, addrMap state)
  where
    encTable = whileJust atomizeConstant popWorkItem


whileJust :: (b -> ConstAtomizationState a) -> ConstAtomizationState (Maybe b) -> ConstAtomizationState ()
whileJust f source = do
  next <- source
  case next of
    Nothing -> return ()
    Just x -> do
      _ <- f x
      whileJust f source



atomizeConstant :: Constant -> ConstAtomizationState ()
atomizeConstant c = case c of
  CNumber n                -> addAtomized [ACNumber n]
  CPlainSymbol sid         -> addAtomized [ACPlainSymbol sid]
  CCompoundSymbol sid args -> atomizeCompoundSymbol sid args
  CMatchData args          -> atomizeMatchData args
  x -> error $ "Unable to encode top-level constant " ++ show x


atomizeCompoundSymbol :: SymId -> [Constant] -> ConstAtomizationState ()
atomizeCompoundSymbol sid args = do
  setReservedSpace (1 + length args)
  let symbolHeader = ACCompoundSymbolHeader (fromIntegral sid) (fromIntegral $ length args)
  atomizedArgs <- mapM atomizeConstArg args
  addAtomized $ symbolHeader : atomizedArgs
  setReservedSpace 0


atomizeMatchData :: [Constant] -> ConstAtomizationState ()
atomizeMatchData args = do
  setReservedSpace (1 + length args)
  let matchHeader = ACMatchHeader (fromIntegral $ length args)
  atomizedArgs <- mapM atomizeConstArg args
  addAtomized $ matchHeader : atomizedArgs
  setReservedSpace $ 0


atomizeConstArg :: Constant -> ConstAtomizationState AtomicConstant
atomizeConstArg c = case c of
  CNumber n        -> return $ ACNumber n
  CPlainSymbol sid -> return $ ACPlainSymbol sid
  CMatchVar n      -> return $ ACMatchVar n
  ds@(CCompoundSymbol _ _) -> do
                addr <- nextFreeAddress
                pushWorkItem ds
                return $ ACCompoundSymbolRef addr
  x -> error $ "Unable to encode constant as argument: " ++ show x



----- State

data ConstAtomizationEnv = ConstAtomizationEnv {
  constants         :: [Constant]
, workQueue         :: [Constant]
, addrMap           :: IntMap.IntMap VMWord
, atomized          :: [AtomicConstant] -- should be a Sequence
, reservedSpace     :: Int
, numAtomizedConsts :: Int
}

emptyConstAtomizationEnv :: [Constant] -> ConstAtomizationEnv
emptyConstAtomizationEnv ctable = ConstAtomizationEnv {
  constants         = ctable
, workQueue         = []
, addrMap           = IntMap.fromList []
, atomized          = []
, reservedSpace     = 0
, numAtomizedConsts = 0
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

addAddrMapping :: Int -> VMWord -> ConstAtomizationState ()
addAddrMapping src dest = do
  state <- get
  let newMap = IntMap.insert src dest (addrMap state)
  put $ state { addrMap = newMap }


pushWorkItem :: Constant -> ConstAtomizationState ()
pushWorkItem c = do
  state <- get
  let workQ = workQueue state
  put $ state { workQueue = workQ ++ [c] }


nextFreeAddress :: ConstAtomizationState Int
nextFreeAddress = do
  state <- get
  let used = length $ atomized state
  let reserved = reservedSpace state
  let pendingItems = workQueue state
  let pending = foldl (\acc c -> acc + (spaceNeededByConstant c)) 0 pendingItems
  return $ used + reserved + pending


spaceNeededByConstant :: Constant -> Int
spaceNeededByConstant c = case c of
  CNumber _              -> 1
  CPlainSymbol _         -> 1
  CMatchVar _            -> 1
  CCompoundSymbol _ args -> 1 + length args
  CMatchData args        -> 1 + length args


addAtomized :: [AtomicConstant] -> ConstAtomizationState ()
addAtomized atoms = do
  state <- get
  put $ state { atomized = (atomized state) ++ atoms }


setReservedSpace :: Int -> ConstAtomizationState ()
setReservedSpace n = do
  state <- get
  put $ state { reservedSpace = n }



----- Byte encoding for data

data AtomicConstant =
    ACPlainSymbol SymId
  | ACCompoundSymbolRef ConstAddr
  | ACCompoundSymbolHeader SymId Int
  | ACNumber Int
  | ACMatchHeader Int
  | ACMatchVar Int
  deriving (Show, Eq)


encodeConstant :: AtomicConstant -> VMWord
encodeConstant c = case c of
  ACPlainSymbol sid            -> Enc.encodePlainSymbol $ fromIntegral sid
  ACCompoundSymbolRef addr     -> Enc.encodeCompoundSymbolRef $ fromIntegral addr
  ACCompoundSymbolHeader sid n -> Enc.encodeCompoundSymbolHeader (fromIntegral sid) (fromIntegral n)
  ACNumber n                   -> Enc.encodeNumber $ fromIntegral n
  ACMatchHeader n              -> Enc.encodeMatchHeader $ fromIntegral n
  ACMatchVar n                 -> Enc.encodeMatchVar $ fromIntegral n


