module Language.Spot.VM.TacAsm (
  assemble
, assembleWithEncodedConstTable
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

-- TODO do we encode nested symbols depth-first or breadth-first? Try both and measure performance!

data AtomicConstant = 
    ACNumber Int
  | ACSymbol SymId
  | ACDataSymbol SymId [AtomicConstant]
  | ACMatchHeader Int
  | ACMatchVar Int

-- When encoding ACDataSymbol : Keep a "next free address" variable, which initially points
-- to the space right after the current block. Create a symbolref with that address. Increment
-- next-free-address with the length of the body. Add the body to a todo list. Decode rest of
-- current block. Then continue with next item from todo list.
-- Problem: On the 'top level', data symbols are expanded directly, influencing the position of
-- the next free address. So take that into account when initially calculating the next free addr.



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
    Tac_load_f r0 fi    -> instructionRI   1 (r r0) (funcAddrs !! fi)
    Tac_load_s r0 s     -> instructionRI   2 (r r0) (i s)
    Tac_load_sd r0 a    -> instructionRI   3 (r r0) (addrConv a)
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


data ConstEncodingState = ConstEncodingState {
  nextFreeAddress :: Int
, addrMap :: IntMap.IntMap VMWord
}

-- TODO don't use fromJust
encodeConstTable :: ConstTable -> ([VMWord], Int -> VMWord)
encodeConstTable ctable =
  let (encodedTable, finalState) = runState (encTable ctable) initState in
  (encodedTable,  {- ( (addrMap finalState) IntMap.! ) -} const 0 )
  where
    initState = ConstEncodingState { nextFreeAddress = 0, addrMap = IntMap.empty }
    encTable ctable = do
      encoded <- mapM (encodeTopLevelConst True) ctable
      return $ foldl (++) [] encoded




encodeTopLevelConst noSpaceReserved c = case c of
  CNumber n -> do reserveConstSpace 1; return $ [encNumber (fromIntegral n)]
  CSymbol sid -> do reserveConstSpace 1; return $ [encSymbol (fromIntegral sid)]
  CDataSymbol sid args -> encodeTopLevelDataSymbolConst noSpaceReserved sid args
  CMatchData pats -> encodeMatchDataConst pats


encodeTopLevelDataSymbolConst noSpaceReserved symId args = do
  when noSpaceReserved (do
    let headerSize = 1
    reserveConstSpace $ headerSize + (length args)
    )
  let symbolHeader = encDataSymbolHeader (fromIntegral symId) (fromIntegral $ length args)
  (body, todo) <- encodeSymbolBody args
  let encodedSymbol = symbolHeader : body
  encodedRest <- mapM (encodeTopLevelConst False) todo
  let combinedRest = foldl (++) [] encodedRest
  return $ encodedSymbol ++ combinedRest

  where
    encodeSymbolBody args = do
      encoded <- mapM encodeConst args
      let body = map fst encoded
      let todo = catMaybes $ map snd encoded
      return (body, todo)

encodeConst c = case c of
  CNumber n -> return (encNumber (fromIntegral n), Nothing)
  CSymbol sid -> return (encSymbol (fromIntegral sid), Nothing)
  sym @ (CDataSymbol sid args) -> do
    addr <- gets nextFreeAddress
    reserveConstSpace (1 + length args)
    return (encDataSymbol (fromIntegral addr), Just sym)
  x -> error $ "Can't encode " ++ (show x) ++ " at this level"


-- TODO
encodeMatchDataConst pats = return []

reserveConstSpace len = do
  state <- get
  let addr = nextFreeAddress state
  put $ state { nextFreeAddress = addr + len }



{-
 - foldM? encodeTopLevel nextWorkItem
 -
 - nextWorkItem is either the next ctable entry or another thing pushed into a separate queue
 -
 - nextFreeAddress can calculate the next free address by looking at the work item queue and 
 - the length of already encoded things (and a reservation for the current block)
 -
 -
 -
 -
 -}



{-
  let (encTable, addrMap) = runState (encodeConstTableSt ctable) (IntMap.empty) in
  (encTable, \a -> fromJust $ IntMap.lookup a addrMap)
-}







{-
-- TODO this is all bad and too complicated
encodeConstant :: Constant -> ([AtomicConstant], [AtomicConstant])
encodeConstant c = case c of
  CNumber n   -> ([], [ACNumber n])
  CSymbol sid -> ([], [ACSymbol sid])
  CDataSymbol sid params -> encodeDataSymbol sid params encodeDataSymbolParam
  CMatchData patConsts -> 
    let encoded = map encodePatternConstant patConsts in
    let before = foldl (++) [] (map fst encoded) in
    let body = map snd encoded in
    let encodedMatchData = (ACMatchHeader (length body)) : body in
    (before, encodedMatchData)

encodeDataSymbol sid params encode = 
    let encoded = map encode params in
    let before = foldl (++) [] $ map fst encoded in
    let body = map snd encoded in
    let encodedSym = (ACDataSymbolHeader sid (length body)) : body in
    (before, encodedSym)

encodeDataSymbolParam :: Constant -> ([AtomicConstant], AtomicConstant)
encodeDataSymbolParam c = case c of
  CNumber _ | CSymbol _ -> 
    let (before, body) = encodeConstant c in
    (before, head body) -- TODO this is not good, we're using secret knowledge about the data we receive
  CDataSymbol _ _ -> 
    let (before, body) = encodeConstant c in
    (before ++ body, ACSymbolRef 0) -- TODO add proper address
  x -> error $ "Can't encode symbol parameter: " ++ (show x)


encodePatternConstant c = case c of
  PatCNumber n -> ACNumber n
  PatCSymbol sid -> ACSymbol sid
  PatCDataSymbol sid params -> encodeDataSymbolParam sid params encodePatternSymbolParam
  PatCVar n -> ACMatchVar n

-- TODO
encodePatternSymbolParam c = case c of
  _ -> ([], ACNumber 0)

{ -
encodeConstTableSt :: ConstTable -> State (IntMap.IntMap VMWord) [VMWord]
encodeConstTableSt ctable = do
  foldM  (\acc elem -> case elem of
    CSymbol sid -> acc ++ [encSymbol sid]
    
  ) [] ctable

-}

