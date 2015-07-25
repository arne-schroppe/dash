module Language.Dash.CodeGen.CodeGenState where

-- TODO list exported functions explicitly


import           Control.Monad.State          hiding (state)
import qualified Data.Map                     as Map
import qualified Data.Sequence                as Seq
import           Language.Dash.CodeGen.Limits
import           Language.Dash.IR.Data
import           Language.Dash.IR.Tac
import           Language.Dash.IR.Nst
import           Language.Dash.VM.Types


----- State -----

type CodeGenState a = State CompEnv a

data CompEnv = CompEnv {
                   instructions :: Seq.Seq [Tac]
                 , scopes       :: [CompScope]
                 }


emptyCompEnv :: CompEnv
emptyCompEnv = CompEnv {
                   instructions = Seq.fromList []
                 , scopes = []
                 }


data CompScope = CompScope {
                   bindings               :: Map.Map Name Reg
                 , selfReferenceSlot      :: Maybe Int

                 -- these are all the registers that hold function values which
                 -- can be called directly with Tac_call. Everything else is
                 -- called with Tac_call_cl
                 , directCallRegs         :: [Reg]

                 , compileTimeConstants   :: Map.Map String CompileTimeConstant
                 , nextFreeRegIndex       :: Int
                 } deriving (Show)


makeScope :: Map.Map Name Reg -> CompScope
makeScope initialBindings = CompScope {
               bindings = initialBindings
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
  | CTConstCompoundSymbol ConstAddr  -- only if it exclusively contains other CompileTimeConstants
  | CTConstLambda FuncAddr  -- only if it is not a closure
  deriving (Show)


beginFunction :: [Name] -> [Name] -> CodeGenState FuncAddr
beginFunction freeVars params = do
  state <- get
  let freeVarBindings = Map.fromList (zipWithReg freeVars 0)
  let paramStart = length freeVars
  let paramBindings = Map.fromList (zipWithReg params paramStart)
  -- The order of arguments for union is important. We prefer params over free vars
  let newScope = makeScope $ Map.union paramBindings freeVarBindings
  put $ state { scopes = newScope : (scopes state) }
  checkRegisterLimits
  addr <- addFunctionPlaceholder
  return addr


endFunction :: FuncAddr -> [Tac] -> CodeGenState ()
endFunction funAddr code = do
  replacePlaceholderWithActualCode funAddr code
  modify $ \ state -> state { scopes = (tail $ scopes state) }




bindVar :: String -> Reg -> CodeGenState ()
bindVar "" _ = error "Binding anonymous var"
bindVar name reg = do
  scope <- getScope
  let bindings' = Map.insert name reg (bindings scope)
  putScope $ scope { bindings = bindings' }
  checkRegisterLimits


numBindings :: CodeGenState Int
numBindings = do
  bs <- gets $ bindings.head.scopes
  return $ Map.size bs




-- a placeholder is needed because we might start to encode other functions while encoding
-- a function. So we can't just append the encoded function to the end when we're done
-- with it, because in some situations we already need its address while encoding it. So
-- the placeholder helps us to give the function a fixed address, no matter when it is
-- actually added to the list of functions.
addFunctionPlaceholder :: CodeGenState FuncAddr
addFunctionPlaceholder = do
  state <- get
  let instrs = instructions state
  let nextFunAddr = Seq.length instrs
  let instrs' = instrs Seq.|> []
  put $ state { instructions = instrs' }
  return $ mkFuncAddr nextFunAddr


replacePlaceholderWithActualCode :: FuncAddr -> [Tac] -> CodeGenState ()
replacePlaceholderWithActualCode funcPlaceholderAddr code = do
  state <- get
  let instrs = instructions state
  let index = funcAddrToInt funcPlaceholderAddr
  let instrs' = Seq.update index code instrs -- replace the original function placeholder with the actual code
  put $ state { instructions = instrs' }


getReg :: NstVar -> CodeGenState Reg
getReg (NVar name _) = getRegByName name

getRegByName :: String -> CodeGenState Reg
getRegByName name = do
  maybeReg <- getRegN
  case maybeReg of
    Just r -> return r
    Nothing -> error $ "Unknown identifier " ++ name
  where getRegN = do
          binds <- gets $ bindings.head.scopes
          return $ Map.lookup name binds


-- getReg :: NstVar -> CodeGenState Reg
-- getReg var = case var of
--   NVar _ NConstant -> error "Compiler error"
--   NVar _ NRecursiveVar -> error "Compiler error: Unexpected recursive var"
--   NVar name NFunParam -> do
--     maybeReg <- param name
--     return $ fromMaybe (error $ "Unknown identifier: " ++ name) maybeReg
-- 
--   -- When calling a closure, the first n registers are formal arguments
--   -- and the next m registers are closed-over variables
--   -- TODO document this fact somewhere visible
--   NVar name NFreeVar -> do
--     maybeReg <- freeVar name
--     return $ fromMaybe (error $ "Unknown identifier: " ++ name) maybeReg
-- 
--   NVar name NLocalVar -> do
--     maybeReg <- localVar name
--     return $ fromMaybe (error $ "Unknown identifier: " ++ name) maybeReg


newReg :: CodeGenState Reg
newReg = do
  scope <- getScope
  let nextFree = nextFreeRegIndex scope
  let reg = mkReg nextFree
  let scope' = scope { nextFreeRegIndex = nextFree + 1 }
  putScope scope'
  return reg


-- TODO rename to isRegWithRefToKnownFunction
isDirectCallReg :: Reg -> CodeGenState Bool
isDirectCallReg reg = do
  scope <- getScope
  let dCallRegs = directCallRegs scope
  return $ Prelude.elem reg dCallRegs

-- TODO same here (rename)
addDirectCallReg :: Reg -> CodeGenState ()
addDirectCallReg reg = do
  scope <- getScope
  let dCallRegs = directCallRegs scope
  let dCallRegs' = reg : dCallRegs
  putScope $ scope { directCallRegs = dCallRegs' }


getSelfReference :: CodeGenState (Maybe Int)
getSelfReference = getScope >>= return.selfReferenceSlot

setSelfReferenceSlot :: Int -> CodeGenState ()
setSelfReferenceSlot index = do
  scope <- getScope
  putScope $ scope { selfReferenceSlot = Just index }


resetSelfReferenceSlot :: CodeGenState ()
resetSelfReferenceSlot = do
  scope <- getScope
  putScope $ scope { selfReferenceSlot = Nothing }


getScope :: CodeGenState CompScope
getScope = do
  gets $ head.scopes


putScope :: CompScope -> CodeGenState ()
putScope s = do
  modify $ \ state -> state { scopes = s : (tail $ scopes state) }


addCompileTimeConst :: Name -> CompileTimeConstant -> CodeGenState ()
addCompileTimeConst "" _ = return ()
addCompileTimeConst name c = do
  scope <- getScope
  let consts = compileTimeConstants scope
  let consts' = Map.insert name c consts
  putScope $ scope { compileTimeConstants = consts' }


-- This retrieves values for NConstant. Those are never inside the current scope
getCompileTimeConstInSurroundingScopes :: Name -> CodeGenState CompileTimeConstant
getCompileTimeConstInSurroundingScopes name = do
  scps <- gets scopes
  getCompConst name scps
  where
    getCompConst _ [] = error $ "Compiler error: no compile time constant named '" ++ name ++ "'"
    getCompConst constName scps = do
      let consts = compileTimeConstants $ head scps
      case Map.lookup constName consts of
        Just c -> return c
        Nothing -> getCompConst constName $ tail scps


-- TODO implement argument spilling to avoid this hard limit
checkRegisterLimits :: CodeGenState ()
checkRegisterLimits = do
  bs <- gets $ bindings.head.scopes
  let usedRegs = Map.size bs
  when (usedRegs >= maxRegisters) $ error "Out of free registers"


zipWithIndex :: [a] -> [(a, Int)]
zipWithIndex l = zip l [0..(length l)]

zipWithReg :: [a] -> Int -> [(a, Reg)]
zipWithReg l offset = zip l $ map mkReg [offset..offset + (length l)]
