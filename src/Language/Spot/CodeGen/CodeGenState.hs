module Language.Spot.CodeGen.CodeGenState where


import           Control.Applicative
import           Control.Monad.State
import qualified Data.Map               as Map
import qualified Data.Sequence          as Seq
import           Language.Spot.IR.Data
import           Language.Spot.IR.Nst
import           Language.Spot.IR.Tac
import           Language.Spot.VM.Types


----- State -----
-- TODO put this into a separate file

type CodeGenState a = State CompEnv a

data CompEnv = CompEnv {
                   instructions :: Seq.Seq [Tac]
                 , scopes       :: [CompScope]
                 }

emptyCompEnv = CompEnv {
                   instructions = Seq.fromList []
                 , scopes = []
                 }

data CompScope = CompScope {
                   functionParams         :: Map.Map String Int
                 , freeVariables          :: Map.Map String Int
                 , forwardDeclaredLambdas :: [String]
                 , selfReferenceSlot      :: Maybe Int

                 -- these are all the registers that hold function values which
                 -- can be called directly with Tac_call. Everything else is
                 -- called with Tac_call_cl
                 , directCallRegs         :: [Int]

                 , compileTimeConstants   :: Map.Map String CompileTimeConstant
                 }

makeScope freeVars params = CompScope {
               functionParams = params
             , freeVariables = freeVars
             , forwardDeclaredLambdas = []
             , selfReferenceSlot = Nothing
             , directCallRegs = []
             , compileTimeConstants = Map.empty
             }

data CompileTimeConstant =
    CTConstNumber VMWord
  | CTConstPlainSymbol SymId
  | CTConstCompoundSymbol ConstAddr
  | CTConstLambda FunAddr


beginFunction :: [String] -> [String] -> CodeGenState Int
beginFunction freeVars params = do
  state <- get
  let localFreeVars = Map.fromList (zipWithIndex freeVars)
  let paramBindings = Map.fromList (zipWithIndex params)
  let newScope = makeScope localFreeVars paramBindings
  put $ state { scopes = newScope : (scopes state) }
  addr <- addPlaceholderFunction
  return addr


endFunction :: Int -> [Tac] -> CodeGenState ()
endFunction funAddr code = do
  state <- get
  let instrs = instructions state
  let instrs' = Seq.update funAddr code instrs
  put $ state { scopes = (tail $ scopes state),
                instructions = instrs' }


numParameters :: CodeGenState Int
numParameters = do
  localParams <- gets $ functionParams.head.scopes
  return $ Map.size localParams


param :: String -> CodeGenState (Maybe Int)
param name = do
  localParams <- gets $ functionParams.head.scopes
  let res = Map.lookup name localParams
  return res


numFreeVars :: CodeGenState Int
numFreeVars = do
  localFreeVars <- gets $ freeVariables.head.scopes
  return $ Map.size localFreeVars


freeVar :: String -> CodeGenState (Maybe Int)
freeVar name = do
  localFreeVars <- gets $ freeVariables.head.scopes
  let res = Map.lookup name localFreeVars
  return res


addPlaceholderFunction :: CodeGenState Int
addPlaceholderFunction = do
  state <- get
  let instrs = instructions state
  let nextFunAddr = Seq.length instrs
  let instrs' = instrs Seq.|> []
  put $ state { instructions = instrs' }
  return nextFunAddr


getRegByName :: String -> CodeGenState Int
getRegByName name = do
  lookup <- getRegN name
  case lookup of
    Just index -> return index
    Nothing -> error $ "Unknown identifier " ++ name
  where getRegN name = do
  -- TODO make this less ugly
          p <- freeVar name
          case p of
                  Just i -> return $ Just i
                  Nothing -> do
                          p <- param name
                          numFree <- numFreeVars
                          return $ (+) <$> p <*> Just numFree


getReg :: NstVar -> CodeGenState Int
getReg (NConstantFreeVar _) = error "Compiler error"

getReg (NFunParam name) = do
  numFree <- numFreeVars
  lookup <- param name
  case lookup of
          Just index -> return $ numFree + index
          Nothing -> error $ "Unknown parameter: " ++ name

-- When calling a closure, the first n registers are formal arguments
-- and the next m registers are closed-over variables
-- TODO document this fact somewhere visible
getReg (NDynamicFreeVar name) = do
  lookup <- freeVar name
  case lookup of
          Just index -> return index
          Nothing -> error $ "Unknown free var: " ++ name

getReg (NLocalVar tmpVar name) = do
  numFree <- numFreeVars
  numParams <- numParameters
  return $ numFree + numParams + tmpVar

getReg (NRecursiveVar name) = error "Compiler error: Unexpected recursive var"


isDirectCallReg :: Reg -> CodeGenState Bool
isDirectCallReg reg = do
  scope <- getScope
  let dCallRegs = directCallRegs scope
  return $ Prelude.elem reg dCallRegs


addDirectCallReg :: Reg -> CodeGenState ()
addDirectCallReg reg = do
  scope <- getScope
  let dCallRegs = directCallRegs scope
  let dCallRegs' = reg : dCallRegs
  putScope $ scope { directCallRegs = dCallRegs' }


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
  state <- get
  return $ head (scopes state)


putScope :: CompScope -> CodeGenState ()
putScope s = do
  state <- get
  put $ state { scopes = s : (tail $ scopes state) }


addCompileTimeConst :: String -> CompileTimeConstant -> CodeGenState ()
addCompileTimeConst "" _ = return ()
addCompileTimeConst name c = do
  scope <- getScope
  let consts = compileTimeConstants scope
  let consts' = Map.insert name c consts
  putScope $ scope { compileTimeConstants = consts' }


-- This retrieves values for NConstantFreeVar. Those are never inside the current scope
getCompileTimeConstInOuterScope :: String -> CodeGenState CompileTimeConstant
getCompileTimeConstInOuterScope name = do
  scps <- gets scopes
  getCompConst name scps
  where
    getCompConst name [] = error $ "Compiler error: no compile time constant named '" ++ name ++ "'"
    getCompConst name scps = do
      let consts = compileTimeConstants $ head scps
      case Map.lookup name consts of
        Just c -> return c
        Nothing -> getCompConst name $ tail scps




zipWithIndex :: [a] -> [(a, Int)]
zipWithIndex l = zip l [0..(length l)]
