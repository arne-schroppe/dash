module Language.Spot.Asm.DataAssembler (
  encodeConstTable
, atomizeConstTable
, AtomicConstant(..)
) where

import           Language.Spot.IR.Data
import Control.Monad.State
import qualified Data.IntMap            as IntMap
import qualified Language.Spot.VM.Bits  as Bits
import           Language.Spot.VM.Types

-- TODO explain the algorithm or simplify it



type ConstAddressMap = Int -> VMWord
type ConstAtomizationState a = State ConstAtomizationEnv a


encodeConstTable :: ConstTable -> ([VMWord], ConstAddressMap)
encodeConstTable ctable =
  let (atoms, mapping) = atomizeConstTable ctable in
  (map encodeConstant atoms, (mapping IntMap.!) )


atomizeConstTable :: ConstTable -> ([AtomicConstant], IntMap.IntMap VMWord)
atomizeConstTable ctable =
  let state = execState (encTable ctable) (emptyConstAtomizationEnv ctable) in
  (atomized state, addrMap state)
  where
    encTable ctable = whileJust atomizeConstant popWorkItem


whileJust :: (b -> ConstAtomizationState a) -> ConstAtomizationState (Maybe b) -> ConstAtomizationState ()
whileJust f source = do
  next <- source
  case next of
    Nothing -> return ()
    Just x -> do
      f x
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

emptyConstAtomizationEnv ctable = ConstAtomizationEnv {
  constants         = ctable
, workQueue         = []
, addrMap           = IntMap.fromList []
, atomized          = []
, reservedSpace     = 0
, numAtomizedConsts = 0
}


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
  ACPlainSymbol sid            -> Bits.encodePlainSymbol $ fromIntegral sid
  ACCompoundSymbolRef addr     -> Bits.encodeCompoundSymbolRef $ fromIntegral addr
  ACCompoundSymbolHeader sid n -> Bits.encodeCompoundSymbolHeader (fromIntegral sid) (fromIntegral n)
  ACNumber n                   -> Bits.encodeNumber $ fromIntegral n
  ACMatchHeader n              -> Bits.encodeMatchHeader $ fromIntegral n
  ACMatchVar n                 -> Bits.encodeMatchVar $ fromIntegral n


