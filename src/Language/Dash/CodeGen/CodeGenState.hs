module Language.Dash.CodeGen.CodeGenState where

-- TODO list exported functions explicitly


import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.State          hiding (state)
import           Data.List
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromJust)
import qualified Data.Sequence                as Seq
import           Language.Dash.Constants
import           Language.Dash.Internal.Error (CompilationError (..))
import           Language.Dash.IR.Data
import           Language.Dash.IR.Nst
import           Language.Dash.IR.Opcode
import           Language.Dash.VM.Types



type CodeGenT a m = StateT CompState (ExceptT CompilationError m) a
type CodeGen a = CodeGenT a Identity

data CompState = CompState
  { instructions    :: Seq.Seq [Opcode]
  , constTable      :: ConstTable
  , symbolNames     :: SymbolNameList
  , moduleIdCounter :: Int
  , scopes          :: [CompScope]
  }


makeCompState :: ConstTable -> SymbolNameList -> CompState
makeCompState ct sns = CompState
  { instructions = Seq.fromList []
  , constTable = ct
  , symbolNames = sns
  , moduleIdCounter = 0
  , scopes = []
  }


data CompScope = CompScope
  { bindings             :: Map.Map Name Reg
  , selfReferenceSlot    :: Maybe Int

  -- these are all the registers that hold function values which
  -- can be called directly with Op_call. Everything else is
  -- called with Op_call_cl
  , directCallRegs       :: [Reg]
  , compileTimeConstants :: Map.Map String CompileTimeConstant
  , nextFreeRegIndex     :: Int
  } deriving (Show)


makeScope :: Map.Map Name Reg -> CompScope
makeScope initialBindings = CompScope
  { bindings = initialBindings
  , selfReferenceSlot = Nothing
  , directCallRegs = []
  , compileTimeConstants = Map.empty
  , nextFreeRegIndex = Map.size initialBindings
  }


-- This helps us to keep track of constant values in the code. They overlap
-- with the constants in the ConstTable, but are not the same. CompileTimeConstants
-- are for example used in determining free variables in closures. Even though a
-- non-local function g is used inside a function f, doesn't add g as a free variable
-- to f. It is only if g itself is a closure (and thereby *not* a CompileTimeConst),
-- that we add it as a free variable. So a CompileTimeConstant has a static representation
-- at runtime and can be used directly, wherever it occurs. Everything else is dynamic
-- data and needs to be passed around
data CompileTimeConstant =
    CTConstNumber VMWord
  | CTConstPlainSymbol SymId
  | CTConstCompoundSymbol ConstAddr -- only if it only contains other CompileTimeConstants
  | CTConstLambda FuncAddr  -- only if it is not a closure
  deriving (Show)


beginFunction :: [Name] -> [Name] -> CodeGen FuncAddr
beginFunction freeVars params = do
  state <- get
  let freeVarBindings = Map.fromList (zipWithReg freeVars 0)
  let paramStart = length freeVars
  let paramBindings = Map.fromList (zipWithReg params paramStart)
  -- The order of arguments for union is important. We prefer params over free vars
  let newScope = makeScope $ Map.union paramBindings freeVarBindings
  put $ state { scopes = newScope : scopes state }
  checkRegisterLimits
  addr <- addFunctionPlaceholder
  return addr


endFunction :: FuncAddr -> [Opcode] -> CodeGen ()
endFunction funAddr code = do
  replacePlaceholderWithActualCode funAddr code
  modify $ \ state -> state { scopes = tail $ scopes state }


bindVar :: String -> Reg -> CodeGen ()
bindVar "" _ = throwError $ InternalCompilerError "Binding anonymous var"
bindVar name reg = do
  scope <- getScope
  let bindings' = Map.insert name reg (bindings scope)
  putScope $ scope { bindings = bindings' }
  checkRegisterLimits


numBindings :: CodeGen Int
numBindings = do
  bs <- gets $ bindings.head.scopes
  return $ Map.size bs


-- a placeholder is needed because we might start to encode other functions while encoding
-- a function. So we can't just append the encoded function to the end when we're done
-- with it, because in some situations we already need its address while encoding it. So
-- the placeholder helps us to give the function a fixed address, no matter when it is
-- actually added to the list of functions.
addFunctionPlaceholder :: CodeGen FuncAddr
addFunctionPlaceholder = do
  state <- get
  let instrs = instructions state
  let nextFunAddr = Seq.length instrs
  let instrs' = instrs Seq.|> []
  put $ state { instructions = instrs' }
  return $ mkFuncAddr nextFunAddr


replacePlaceholderWithActualCode :: FuncAddr -> [Opcode] -> CodeGen ()
replacePlaceholderWithActualCode funcPlaceholderAddr code = do
  state <- get
  let instrs = instructions state
  let index = funcAddrToInt funcPlaceholderAddr
  -- replace the original function placeholder with the actual code
  let instrs' = Seq.update index code instrs
  put $ state { instructions = instrs' }


getReg :: NstVar -> CodeGen Reg
getReg (NVar name _) = getRegByName name


getRegByName :: String -> CodeGen Reg
getRegByName name = do
  maybeReg <- getRegN
  case maybeReg of
    Just r -> return r
    Nothing -> throwError $ CodeError $ "Unknown identifier " ++ name
  where getRegN = do
          binds <- gets $ bindings.head.scopes
          return $ Map.lookup name binds


newReg :: CodeGen Reg
newReg = do
  scope <- getScope
  let nextFree = nextFreeRegIndex scope
  let reg = mkReg nextFree
  let scope' = scope { nextFreeRegIndex = nextFree + 1 }
  putScope scope'
  return reg


-- TODO rename to isRegWithRefToKnownFunction
isDirectCallReg :: Reg -> CodeGen Bool
isDirectCallReg reg = do
  scope <- getScope
  let dCallRegs = directCallRegs scope
  return $ Prelude.elem reg dCallRegs


-- TODO same here (rename)
addDirectCallReg :: Reg -> CodeGen ()
addDirectCallReg reg = do
  scope <- getScope
  let dCallRegs = directCallRegs scope
  let dCallRegs' = reg : dCallRegs
  putScope $ scope { directCallRegs = dCallRegs' }


getSelfReference :: CodeGen (Maybe Int)
getSelfReference = liftM selfReferenceSlot getScope


setSelfReferenceSlot :: Int -> CodeGen ()
setSelfReferenceSlot index = do
  scope <- getScope
  putScope $ scope { selfReferenceSlot = Just index }


resetSelfReferenceSlot :: CodeGen ()
resetSelfReferenceSlot = do
  scope <- getScope
  putScope $ scope { selfReferenceSlot = Nothing }


getScope :: CodeGen CompScope
getScope =
  gets $ head.scopes


putScope :: CompScope -> CodeGen ()
putScope s =
  modify $ \ state -> state { scopes = s : tail (scopes state) }


addCompileTimeConst :: Name -> CompileTimeConstant -> CodeGen ()
addCompileTimeConst "" _ = return ()
addCompileTimeConst name c = do
  scope <- getScope
  let consts = compileTimeConstants scope
  let consts' = Map.insert name c consts
  putScope $ scope { compileTimeConstants = consts' }


-- This retrieves values for NConstant. Those are never inside the current scope
getCompileTimeConstInSurroundingScopes :: Name -> CodeGen CompileTimeConstant
getCompileTimeConstInSurroundingScopes name = do
  scps <- gets scopes
  getCompConst name scps
  where
    getCompConst _ [] = throwError $ InternalCompilerError $ 
                                "No compile time constant named '" ++ name ++ "'"
    getCompConst constName scps = do
      let consts = compileTimeConstants $ head scps
      case Map.lookup constName consts of
        Just c -> return c
        Nothing -> getCompConst constName $ tail scps


-- TODO implement argument spilling to avoid this hard limit
checkRegisterLimits :: CodeGen ()
checkRegisterLimits = do
  bs <- gets $ bindings.head.scopes
  let usedRegs = Map.size bs
  when (usedRegs >= maxRegisters) $
          throwError $ InternalCompilerError "Out of free registers"


zipWithIndex :: [a] -> [(a, Int)]
zipWithIndex l = zip l [0..(length l)]

zipWithReg :: [a] -> Int -> [(a, Reg)]
zipWithReg l offset = zip l $ map mkReg [offset..offset + length l]


-- TODO this is basically copied form normalization-state, unify it
addConstant :: Constant -> CodeGen ConstAddr
addConstant c = do
  state <- get
  let cTable = constTable state
  let nextAddr = mkConstAddr $ length cTable
  let constTable' = cTable ++ [c] -- TODO can we cons + reverse?
  put $ state { constTable = constTable' }
  return nextAddr

newModuleIdentifier :: CodeGen SymId
newModuleIdentifier = do
  name <- newName
  symId <- addSymbolName name
  return symId
  where
    newName = do
      index <- gets moduleIdCounter
      modify $ \ env -> env { moduleIdCounter = index + 1 }
      return $ "$mod_" ++ show index


-- TODO also copied from normalization
-- TODO add a second state just for data
addSymbolName :: String -> CodeGen SymId
addSymbolName s = do
  state <- get
  let syms = symbolNames state
  if s `elem` syms then  -- TODO optimize this with a map
    return $ mkSymId $ fromJust $ elemIndex s syms
  else do
    let nextId = mkSymId $ length syms
    let syms' = syms ++ [s]
    put $ state { symbolNames = syms' }
    return nextId


