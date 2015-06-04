module Language.Spot.Asm.DataAssembler (
  encodeConstTable
, atomizeConstTable
, AtomicConstant(..)
) where

import           Language.Spot.IR.Data
import Control.Monad.State
import qualified Data.IntMap            as IntMap
import           Language.Spot.VM.Bits
import           Language.Spot.VM.Types

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


