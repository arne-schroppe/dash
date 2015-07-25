module Language.Dash.CodeGen.CodeGenState where

-- TODO list exported functions explicitly


import           Control.Applicative
import           Control.Monad.State          hiding (state)
import qualified Data.Map                     as Map
import qualified Data.Sequence                as Seq
import           Language.Dash.CodeGen.Limits
import           Language.Dash.IR.Data
import           Language.Dash.IR.Nst
import           Language.Dash.IR.Tac
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
                   functionParams         :: Map.Map Name Reg
                 , freeVariables          :: Map.Map Name Reg
                 , localVariables         :: Map.Map Name Reg
                 , forwardDeclaredLambdas :: [String]
                 , selfReferenceSlot      :: Maybe Int

                 -- these are all the registers that hold function values which
                 -- can be called directly with Tac_call. Everything else is
                 -- called with Tac_call_cl
                 , directCallRegs         :: [Reg]

                 , compileTimeConstants   :: Map.Map String CompileTimeConstant
                 } deriving (Show)


-- TODO save final register locations from the start, don't calculate dynamically
-- also, store as Map Name (Reg, VarType) where VarType = Local | Free | Param etc

makeScope :: Map.Map Name Reg -> Map.Map Name Reg -> CompScope
makeScope freeVars params = CompScope {
               functionParams = params
             , freeVariables = freeVars
             , forwardDeclaredLambdas = []
             , selfReferenceSlot = Nothing
             , directCallRegs = []
             , compileTimeConstants = Map.empty
             , localVariables = Map.empty
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
  let localFreeVars = Map.fromList (zipWithReg freeVars)
  let paramBindings = Map.fromList (zipWithReg params)
  let newScope = makeScope localFreeVars paramBindings
  put $ state { scopes = newScope : (scopes state) }
  checkRegisterLimits
  addr <- addFunctionPlaceholder
  return addr


endFunction :: FuncAddr -> [Tac] -> CodeGenState ()
endFunction funAddr code = do
  replacePlaceholderWithActualCode funAddr code
  modify $ \ state -> state { scopes = (tail $ scopes state) }


numParameters :: CodeGenState Int
numParameters = do
  localParams <- gets $ functionParams.head.scopes
  return $ Map.size localParams


param :: String -> CodeGenState (Maybe Reg)
param name = do
  localParams <- gets $ functionParams.head.scopes
  let res = Map.lookup name localParams
  return res


numFreeVars :: CodeGenState Int
numFreeVars = do
  localFreeVars <- gets $ freeVariables.head.scopes
  return $ Map.size localFreeVars


freeVar :: String -> CodeGenState (Maybe Reg)
freeVar name = do
  localFreeVars <- gets $ freeVariables.head.scopes
  let res = Map.lookup name localFreeVars
  return res


bindLocalVar :: String -> Reg ->  CodeGenState ()
bindLocalVar "" _ = return ()
bindLocalVar name reg = do
  scope <- getScope
  let bindings' = Map.insert name reg (localVariables scope)
  putScope $ scope { localVariables = bindings' }
  checkRegisterLimits

localVar :: String -> CodeGenState (Maybe Reg)
localVar name = do
  localVars <- gets $ localVariables.head.scopes
  return $ Map.lookup name localVars

numLocalVars :: CodeGenState Int
numLocalVars = do
  localVars <- gets $ localVariables.head.scopes
  return $ Map.size localVars




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


getRegByName :: String -> CodeGenState Reg
getRegByName name = do
  maybeReg <- getRegN
  case maybeReg of
    Just r -> return r
    Nothing -> error $ "Unknown identifier " ++ name
  where getRegN = do
          -- TODO rewrite this in a more understandable way
          let pl = liftM2 mplus
          numFree <- numFreeVars
          -- we're trying one possible type of var after another
          freeVar name `pl`
              (param name >>= return . addReg numFree ) `pl`
              localVar name
        addReg offset maybeReg =
            ( \ r -> mkReg $ offset + (regToInt r)) <$> maybeReg


getReg :: NstVar -> CodeGenState Reg
getReg var = case var of
  NConstantFreeVar _ -> error "Compiler error"
  NRecursiveVar _ -> error "Compiler error: Unexpected recursive var"
  NFunParam name -> do
    numFree <- numFreeVars
    maybeReg <- param name
    case maybeReg of
            Just r -> return $ mkReg $ numFree + (regToInt r)
            Nothing -> error $ "Unknown parameter: " ++ name

  -- When calling a closure, the first n registers are formal arguments
  -- and the next m registers are closed-over variables
  -- TODO document this fact somewhere visible
  NDynamicFreeVar name -> do
    maybeReg <- freeVar name
    case maybeReg of
            Just r -> return r
            Nothing -> error $ "Unknown free var: " ++ name

  NLocalVar tmpVar _ -> do
    numFree <- numFreeVars
    numParams <- numParameters
    return $ mkReg $ numFree + numParams + tmpVar



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


-- This retrieves values for NConstantFreeVar. Those are never inside the current scope
getCompileTimeConstInOuterScope :: Name -> CodeGenState CompileTimeConstant
getCompileTimeConstInOuterScope name = do
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
  values <- sequence [numLocalVars, numFreeVars, numParameters]
  let usedRegs = foldr (+) 0 values
  when (usedRegs > maxRegisters) $ error "Out of free registers"


zipWithIndex :: [a] -> [(a, Int)]
zipWithIndex l = zip l [0..(length l)]

zipWithReg :: [a] -> [(a, Reg)]
zipWithReg l = zip l $ map mkReg [0..(length l)]
