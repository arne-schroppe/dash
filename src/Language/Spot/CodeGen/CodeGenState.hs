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
, regContainingVar
, addSymbolName
, addConstants

-- TODO hide these too by moving execState in here
, getOpcodes
, getCTable
, getSymNames
, emptyCode
) where


import Language.Spot.IR.Opcode

import Control.Monad.State
import Control.Applicative
import Control.Lens
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
                 , _ctable :: ConstTable
                 , _symnames :: Map.Map String Word32
                 , _funcContextStack :: [FuncContext]
                 }

data FuncContext = FuncContext { _reservedRegisters :: Word32
                               , _resultRegStack :: [Word32]
                               , _bindings :: Map.Map String Word32
                               }

makeLenses ''Code
makeLenses ''FuncContext


emptyFuncContext = FuncContext { _reservedRegisters = 0
                               , _resultRegStack = []
                               , _bindings = Map.empty
                               }


emptyCode = Code { _opcodes = Seq.fromList []
                 , _ctable = []
                 , _symnames = Map.empty
                 , _funcContextStack = []
                 }

-- Convenience getters

getOpcodes :: Code -> [[Opcode]]
getOpcodes = toList . view opcodes

getCTable :: Code -> ConstTable
getCTable = view ctable

getSymNames :: Code -> SymbolNameList
getSymNames = map fst . sortBy (\a b -> compare (snd a) (snd b)) . Map.toList . view symnames


-- Functions


use' l = fromJust <$> (preuse l)

setFunctionCode :: Int -> [Opcode] -> State Code ()
setFunctionCode funIndex code =
  opcodes.(ix funIndex) .= code

beginFunction = do
  pushFuncContext
  r <- reserveReg -- TODO we should assert that this is always 0
  pushResultReg r
  funAddr <- Seq.length <$> use opcodes
  opcodes `addHeadS` []
  return funAddr

endFunction = do
  popResultReg
  popFuncContext


pushFuncContext :: State Code ()
pushFuncContext = do
  fcStack <- use funcContextStack
  funcContextStack `addHead` emptyFuncContext

popFuncContext :: State Code ()
popFuncContext =
  funcContextStack %= tail


-- Registers

reserveReg :: State Code Word32
reserveReg = do
  numRegs <- use' $ funcContextStack._head.reservedRegisters
  funcContextStack._head.reservedRegisters += 1
  return numRegs

peekReg :: State Code Word32
peekReg = use' $ funcContextStack._head.reservedRegisters

resultReg :: State Code Word32
resultReg = use' $ funcContextStack._head.resultRegStack._head

pushResultReg :: Word32 -> State Code ()
pushResultReg r = (funcContextStack._head.resultRegStack) `addHead` r

popResultReg :: State Code ()
popResultReg = funcContextStack._head.resultRegStack %= tail



-- Variables

addVar :: String -> Word32 -> State Code ()
addVar n r = do
  funcContextStack._head.bindings.(at n) .= Just r

regContainingVar :: String -> State Code Word32
regContainingVar n = (fromJust . join) <$> (preuse $ funcContextStack._head.bindings.(at n))


-- Symbol names and constants

addSymbolName :: String -> State Code Word32
addSymbolName s = do
  syms <- use symnames
  if Map.member s syms then
    return $ syms Map.! s
  else do
    nextId <- (fromIntegral . Map.size) <$> use symnames
    symnames %= (Map.insert s nextId)
    return nextId

addConstants :: [Word32] -> State Code Word32
addConstants cs = do
  nextAddr <- length <$> use ctable
  ctable %= (++ cs)
  return $ fromIntegral nextAddr


addHead lens value = lens %= (value :)
addHeadS lens value = lens %= (value <|)
