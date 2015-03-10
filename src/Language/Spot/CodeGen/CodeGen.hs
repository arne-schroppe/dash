{-# LANGUAGE TemplateHaskell #-}

module Language.Spot.CodeGen.CodeGen (
  compile
) where

import Language.Spot.IR.Ast
import Language.Spot.IR.Opcode
import Language.Spot.VM.Bits

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
import Debug.Trace


-- TODO "Code" is an utterly stupid name for this
data Code = Code { _opcodes :: [[Opcode]]
                 , _ctable :: ConstTable
                 , _symnames :: SymbolNameList
                 , _funcContextStack :: [FuncContext]
                 , _currentFuncStack :: [Int]
                 }

data FuncContext = FuncContext { _reservedRegisters :: Word32
                               , _resultRegStack :: [Word32]
                               , _bindings :: Map.Map String Word32
                               }

makeLenses ''Code
makeLenses ''FuncContext




compile :: Expr -> ([[Opcode]], ConstTable, SymbolNameList)
compile ast = (view opcodes result, view ctable result, reverse $ view symnames result) --todo reverse most of this
  where result = execState (addFunction ast) emptyCode


addFunction :: Expr -> State Code ()
addFunction e = do
  beginFunction
  compileExpression e
  addOpcodes [Op_halt] -- TODO get rid of this, use op_ret
  endFunction

compileExpression e = case e of
  LitNumber n       -> makeLitNumber n
  LitSymbol s vals  -> makeLitSymbol s vals
  FunCall name args -> makeFunCall name args
  LocalBinding (Binding name expr) body -> makeLocalBinding name expr body
  Var a             -> makeVar a
  a -> error $ "Can't compile: " ++ show a

makeLitNumber n = do
  r <- resultReg
  addOpcodes [Op_load_i r (fromIntegral n)]

makeLitSymbol s []   = do
  newId <- addSymbolName s
  r <- resultReg
  addOpcodes [Op_load_s r newId]
makeLitSymbol s args = do
  symId <- addSymbolName s
  r <- resultReg
  let symHeader = encDataSymbolHeader symId (fromIntegral $ length args)
  let symEntry = symHeader : (map encodeAstValue args)
  newAddr <- addConstants symEntry
  addOpcodes [Op_load_sd r newAddr]

makeFunCall (Var "add") (op1:op2:[]) =
  makeMathFunc Op_add op1 op2
makeFunCall (Var "sub") (op1:op2:[]) =
  makeMathFunc Op_sub op1 op2
makeFunCall (Var n) args = do
  resReg <- resultReg
  fr <- registerContainingVar n
  -- TODO the registers for args must come immediately after the one for the
  -- function address. Right now we're not making sure that that happens. Create
  -- op_load_f in here and just store the function address for n
  argRegs <- replicateM (length args) reserveReg
  let regsAndArgs = zip argRegs args
  forM_ regsAndArgs (\(aReg, arg) -> do
    evalArgument arg aReg)
  addOpcodes [ Op_call resReg fr (fromIntegral $ length args) ]
makeFunCall _ _ = error "Unknown function"

makeLocalBinding name (FunDef args expr) body = do
  r <- reserveReg
  funAddr <- makeFunction args expr
  addOpcodes [Op_load_f r funAddr]
  addVar name r
  compileExpression body
makeLocalBinding name expr body = do
  r <- reserveReg
  pushResultReg r
  compileExpression expr
  popResultReg
  addVar name r
  compileExpression body

makeFunction args expr = do
  funAddr <- beginFunction
  forM_ args (\arg -> do
    r <- reserveReg
    addVar arg r)
  compileExpression expr
  endFunction
  return funAddr

makeVar a = do
  r <- resultReg
  r1 <- registerContainingVar a
  addOpcodes [ Op_move r r1 ]

makeMathFunc mf op1 op2 = do
  r <- resultReg
  argRegs <- replicateM 2 reserveReg
  let regsAndArgs = zip argRegs [op1, op2]
  forM_ regsAndArgs (\(aReg, arg) -> do
    evalArgument arg aReg)
  addOpcodes [ mf r (argRegs !! 0) (argRegs !! 1) ]

evalArgument (Var n) r = do
  vr <- registerContainingVar n
  when (vr /= r) $ addOpcodes [ Op_move r vr ]

evalArgument arg r   = do
  pushResultReg r
  compileExpression arg
  popResultReg

encodeAstValue (LitNumber n) = encNumber $ fromIntegral n
encodeAstValue _ = error "can't encode symbol"

beginFunction = do
  pushFuncContext
  r <- reserveReg -- TODO we should assert that this is always 0
  pushResultReg r
  funAddr <- length <$> use opcodes
  -- modifyOpcodes (\opcs -> [] : opcs)
  opcodes %= ([] :)
  return funAddr

endFunction = do
  addOpcodes [ Op_ret ]
  popResultReg
  popFuncContext




------- State



emptyFuncContext = FuncContext { _reservedRegisters = 0
                               , _resultRegStack = []
                               , _bindings = Map.empty
                               }


emptyCode = Code { _opcodes = []
                 , _ctable = []
                 , _symnames = []
                 , _funcContextStack = []
                 , _currentFuncStack = [0]
                 }

reserveReg :: State Code Word32
reserveReg = do
  numRegs <- preuse $ funcContextStack._head.reservedRegisters -- gets $ reservedRegisters . currentFuncCtx
  funcContextStack._head.reservedRegisters += 1
  -- modifyFuncCtx (\st -> st { reservedRegisters = numRegs + 1 })
  return $ fromJust numRegs



resultReg = do
  r <- preuse $ funcContextStack._head.resultRegStack._head
  return $ fromJust r
  -- gets $ head . resultRegStack . currentFuncCtx

-- currentFuncCtx :: Code -> FuncContext
-- currentFuncCtx = head . funcContext

pushResultReg r = do
  -- modifyFuncCtx (\ctx -> ctx { resultRegStack = r : (resultRegStack ctx) })
  funcContextStack._head.resultRegStack %= (r :)

popResultReg = do
  -- modifyFuncCtx (\ctx -> ctx { resultRegStack = tail $ resultRegStack ctx })
  funcContextStack._head.resultRegStack %= tail




pushFuncContext = do
  fcStack <- use funcContextStack
  funcContextStack %= (emptyFuncContext :)
  currentFuncStack %= ((length fcStack) :)
  
{-
  modify (\st -> st {
    funcContext = emptyFuncContext : (funcContext st),
    currentFunc = (length $ funcContext st) : (currentFunc st) })
-}

popFuncContext = do
  funcContextStack %= tail
  currentFuncStack %= tail
{-
  modify (\st -> st { 
    funcContext = tail $ funcContext st,
    currentFunc = tail $ currentFunc st })
-}

addVar n r = do
  funcContextStack._head.bindings %= (Map.insert n r)
  -- modifyFuncCtx (\st -> st { bindings = Map.insert n r $ bindings st })

registerContainingVar n = do
  binds <- use $ funcContextStack._head.bindings
  return $ fromJust ((Map.lookup n (binds :: Map.Map String Word32) ) :: Maybe Word32)
  -- gets $ fromJust . Map.lookup n . bindings . currentFuncCtx

addOpcodes opcs = do
  currentF <- preuse $ currentFuncStack._head
  opcodes.(element $ fromJust currentF) %= (++ opcs)
  -- modifyCurrentFunc (++ opcs)

{-
currentFunc c =
  let currentF = (view (currentFuncStack._head) c) :: Sum Int in
  opcodes . (element (getSum currentF))
-}

addSymbolName :: String -> State Code Word32
addSymbolName s = do
  nextId <- length <$> use symnames
  symnames %= (s :)
  return $ fromIntegral nextId

addConstants :: [Word32] -> State Code Word32
addConstants cs = do
  nextAddr <- length <$> use ctable
  ctable %= (++ cs)
  return $ fromIntegral nextAddr

{-
getCodeFieldLength field = gets $ length . field

changeHead f l = (f $ head l) : tail l

modifyOpcodes f = modify (\st -> st { opcodes = f (opcodes st) })

modifyCurrentFunc f = modify (\st ->
  let (pre, post) = splitAt (head . currentFunc $ st) (opcodes st) in
  st { opcodes = pre ++ [f (head post)] ++ (tail post) }) --TODO use a sequence!

modifySymNames f = modify (\st -> st { symnames = f (symnames st) })

modifyConsts f = modify (\st -> st { ctable = f (ctable st) })

modifyFuncCtx f = modify (\st -> st { funcContext = changeHead f (funcContext st) })

-}
