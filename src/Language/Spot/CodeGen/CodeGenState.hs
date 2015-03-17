{-# LANGUAGE TemplateHaskell #-}

module Language.Spot.CodeGen.CodeGenState (
  setFunctionCode
, beginFunction
, endFunction
, reserveReg
, peekReg
, resultReg
, pushResultReg
, popResultReg
, addVar
, addArguments
, regContainingVar
, addSymbolName
, addConstants

, pushSubContext
, popSubContext

-- TODO hide these too by moving execState in here
, getOpcodes
, getCTable
, getSymNames
, emptyCode
) where


import Language.Spot.IR.Opcode

import Control.Monad.State
import Control.Applicative
import Control.Lens hiding (Context)
import Control.Lens.Zoom
import Data.List
import Data.List.Lens
import Data.Word
import Data.Maybe
import Data.Monoid
import Data.Foldable
import Control.Exception.Base
import qualified Data.Sequence as Seq
import qualified Data.Map as Map


-- TODO don't use fromJust
-- TODO "Code" is an utterly stupid name for this
data Code = Code { _opcodes :: Seq.Seq [Opcode]
                 , _constTable :: ConstTable
                 , _symbolNames :: Map.Map String Word32
                 , _funcRegDataStack :: [FunctionRegisterData]
                 , _contextStack :: [Context]
                 }

-- reservedRegisters is a stack because we can have sub-contexts inside a function
data FunctionRegisterData = FunctionRegisterData { _reservedRegisters :: [Word32]
                                                 , _resultRegStack :: [Word32]
                                                 }

data Context = Context { _bindings :: Map.Map String Word32 }

makeLenses ''Code
makeLenses ''Context
makeLenses ''FunctionRegisterData


emptyFuncRegisterData = FunctionRegisterData { _reservedRegisters = [0]
                                             , _resultRegStack = []
                                             }

emptyContext = Context { _bindings = Map.empty }


emptyCode = Code { _opcodes = Seq.fromList []
                 , _constTable = []
                 , _symbolNames = Map.empty
                 , _funcRegDataStack = []
                 , _contextStack = []
                 }

-- Convenience getters

getOpcodes :: Code -> [[Opcode]]
getOpcodes = toList . view opcodes

getCTable :: Code -> ConstTable
getCTable = view constTable

getSymNames :: Code -> SymbolNameList
getSymNames = map fst . sortBy (\a b -> compare (snd a) (snd b)) . Map.toList . view symbolNames


-- Functions


-- TODO use StateT in type declarations
use' l = fromJust <$> (preuse l)

setFunctionCode :: Int -> [Opcode] -> State Code ()
setFunctionCode funIndex code =
  opcodes.(ix funIndex) .= code

beginFunction :: [String] -> State Code Int
beginFunction args = do
  pushFuncRegisters
  pushContext
  r <- reserveReg -- TODO we should assert that this is always 0
  pushResultReg r
  addArguments args
  funAddr <- Seq.length <$> use opcodes
  opcodes `addHeadS` []
  return funAddr

endFunction = do
  popResultReg
  popContext
  popFuncRegisters


pushFuncRegisters :: State Code ()
pushFuncRegisters =
  funcRegDataStack `addHead` emptyFuncRegisterData

popFuncRegisters :: State Code ()
popFuncRegisters =
  funcRegDataStack %= tail

pushContext :: State Code ()
pushContext =
  contextStack `addHead` emptyContext

popContext :: State Code ()
popContext =
  contextStack %= tail

pushSubContext :: State Code ()
pushSubContext = do
  pushContext
  reservedRegs <- use' $ funcRegDataStack._head.reservedRegisters._head
  (funcRegDataStack._head.reservedRegisters) `addHead` reservedRegs

popSubContext :: State Code ()
popSubContext = do
  contextStack %= tail
  funcRegDataStack._head.reservedRegisters %= tail


-- Registers

reserveReg :: State Code Word32
reserveReg = do
  numRegs <- use' $ funcRegDataStack._head.reservedRegisters._head
  funcRegDataStack._head.reservedRegisters._head += 1
  return numRegs

peekReg :: State Code Word32
peekReg = use' $ funcRegDataStack._head.reservedRegisters._head

resultReg :: State Code Word32
resultReg = use' $ funcRegDataStack._head.resultRegStack._head

pushResultReg :: Word32 -> State Code ()
pushResultReg r = (funcRegDataStack._head.resultRegStack) `addHead` r

popResultReg :: State Code ()
popResultReg = funcRegDataStack._head.resultRegStack %= tail



-- Variables

addVar :: String -> Word32 -> State Code ()
addVar n r = do
  contextStack._head.bindings.(at n) .= Just r

addArguments :: [String] -> State Code ()
addArguments ns =
  Control.Monad.State.forM_ ns (\n -> do
    r <- reserveReg
    addVar n r)

regContainingVar :: String -> State Code Word32
regContainingVar n = (fromJust . join) <$> (preuse $ contextStack._head.bindings.(at n))


-- Symbol names and constants

addSymbolName :: String -> State Code Word32
addSymbolName s = do
  syms <- use symbolNames
  if Map.member s syms then
    return $ syms Map.! s
  else do
    nextId <- (fromIntegral . Map.size) <$> use symbolNames
    symbolNames %= (Map.insert s nextId)
    return nextId

addConstants :: [Word32] -> State Code Word32
addConstants cs = do
  nextAddr <- length <$> use constTable
  constTable %= (++ cs)
  return $ fromIntegral nextAddr


addHead lens value = lens %= (value :)
addHeadS lens value = lens %= (value <|)


