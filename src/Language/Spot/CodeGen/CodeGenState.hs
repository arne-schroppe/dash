{-# LANGUAGE TemplateHaskell #-}

module Language.Spot.CodeGen.CodeGenState where


import Language.Spot.IR.Opcode

import Control.Monad.State
import Control.Applicative
import Control.Lens
import Control.Lens.Zoom
import Data.List.Lens
import Data.Word
import Data.Maybe
import Data.Monoid
import Control.Exception.Base
import qualified Data.Map as Map


-- TODO "Code" is an utterly stupid name for this
data Code = Code { _opcodes :: [[Opcode]]
                 , _ctable :: ConstTable
                 , _symnames :: SymbolNameList
                 , _funcContextStack :: [FuncContext]
                 , _editedFunctionIndexStack :: [Int]
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


emptyCode = Code { _opcodes = []
                 , _ctable = []
                 , _symnames = []
                 , _funcContextStack = []
                 , _editedFunctionIndexStack = [0]
                 }



beginFunction = do
  pushFuncContext
  r <- reserveReg -- TODO we should assert that this is always 0
  pushResultReg r
  funAddr <- length <$> use opcodes
  opcodes %= ([] :)
  return funAddr

endFunction = do
  addOpcodes [ Op_ret ]
  popResultReg
  popFuncContext



reserveReg :: State Code Word32
reserveReg = do
  numRegs <- fromJust <$> (preuse $ funcContextStack._head.reservedRegisters)
  funcContextStack._head.reservedRegisters += 1
  return numRegs


resultReg :: State Code Word32
resultReg = fromJust <$> (preuse $ funcContextStack._head.resultRegStack._head)


pushResultReg :: Word32 -> State Code ()
pushResultReg r = (funcContextStack._head.resultRegStack) `addHead` r

popResultReg :: State Code ()
popResultReg = funcContextStack._head.resultRegStack %= tail

pushFuncContext :: State Code ()
pushFuncContext = do
  fcStack <- use funcContextStack
  funcContextStack `addHead` emptyFuncContext
  editedFunctionIndexStack `addHead` length fcStack

popFuncContext :: State Code ()
popFuncContext = do
  funcContextStack %= tail
  editedFunctionIndexStack %= tail

addHead lens value = lens %= (value :)

addVar n r = do
  funcContextStack._head.bindings %= (Map.insert n r)

registerContainingVar n = do
  binds <- use $ funcContextStack._head.bindings
  return $ fromJust (Map.lookup n binds)

addOpcodes opcs = do
  currentF <- fromJust <$> (preuse $ editedFunctionIndexStack._head)
  opcodes.(element currentF) %= (++ opcs)


addSymbolName :: String -> State Code Word32
addSymbolName s = do
  nextId <- length <$> use symnames
  symnames `addHead` s
  return $ fromIntegral nextId

addConstants :: [Word32] -> State Code Word32
addConstants cs = do
  nextAddr <- length <$> use ctable
  ctable %= (++ cs)
  return $ fromIntegral nextAddr
