module Language.Spot.CodeGen.CodeGen2 (
  compile
) where

import Language.Spot.IR.Norm
import Language.Spot.IR.Tac
import Language.Spot.VM.Types

import Control.Monad.State
import qualified Data.Sequence as Seq
import Data.Foldable
import Control.Applicative

import Debug.Trace

import qualified Data.Sequence as Seq
import qualified Data.Map as Map

-- TODO when there is more time, do dataflow analysis to reuse registers



compile :: NormExpr -> ConstTable -> SymbolNameList -> ([[Tac Reg]], ConstTable, SymbolNameList)
compile expr _ symlist =
  let result = execState (compileFunc [] [] expr) emptyCompState in
  (toList (instructions result), dataTable result, symlist)


compileFunc freeVars params expr = do
  funAddr <- beginFunction freeVars params
  funcCode <- compileExpr expr
  endFunction funAddr funcCode
  return funAddr

compileExpr expr = case expr of
  NLet var atom body -> compileLet var atom body
  NAtom a -> do
          code <- compileAtom 0 a "" True
          return $ code ++ [Tac_ret 0]


compileAtom reg atom name isResultValue = case atom of
  NNumber n -> do
          addCodeConst name $ CConstNumber (fromIntegral n)
          return [Tac_load_i reg (fromIntegral n)]
  NPlainSymbol sid -> do
          addCodeConst name $ CConstPlainSymbol sid
          return [Tac_load_ps reg sid]
  NPrimOp (NPrimOpAdd a b) -> do
          ra <- getReg a
          rb <- getReg b
          return [Tac_add reg ra rb]
  NPrimOp (NPrimOpSub a b) -> do
          ra <- getReg a
          rb <- getReg b
          return [Tac_sub reg ra rb]
  NLambda [] params expr -> do
          funAddr <- compileFunc [] params expr
          addCodeConst name $ CConstLambda funAddr
          compileLoadLambda reg funAddr isResultValue
  NLambda freeVars params expr -> compileClosure reg freeVars params expr
  NFunCall funVar args -> do
          argInstrs <- mapM (uncurry compileSetArg) $ zipWithIndex args
          rFun <- getReg funVar
          direct <- isDirectCallReg rFun
          if direct then do
            let callInstr = [Tac_call reg rFun (length args)]
            return $ argInstrs ++ callInstr
          else do
            let callInstr = [Tac_call_cl reg rFun (length args)]
            return $ argInstrs ++ callInstr
  NVar var -> case var of
          NLocalVar varId _ -> do
                 r <- getReg var
                 return [Tac_move reg r]
          NFunParam name -> do
                 r <- getReg var
                 return [Tac_move reg r]
          NConstantFreeVar name -> compileConstantFreeVar reg name isResultValue
          _ -> error "fail"
  x -> error $ "Unable to compile " ++ (show x)

compileConstantFreeVar :: Reg -> String -> Bool -> State CompState [Tac Reg]
compileConstantFreeVar reg name isResultValue = do
  codeConst <- getCodeConstInOuterScope name
  case codeConst of
          CConstNumber n -> return [Tac_load_i reg (fromIntegral n)]
          CConstPlainSymbol symId -> return [Tac_load_ps reg symId]
          -- CConstCompoundSymbol ConstAddr
          CConstLambda funAddr -> compileLoadLambda reg funAddr isResultValue

compileLoadLambda reg funAddr isResultValue = do
  let ldFunAddr = [Tac_load_f reg funAddr]
  if isResultValue then
      return $ ldFunAddr ++ [Tac_make_cl reg reg 0]
  else
      return $ ldFunAddr

compileLet tmpVar@(NLocalVar tmpId name) atom body =
  compileLet' tmpVar atom name body

compileLet tmpVar atom body =
  compileLet' tmpVar atom "" body

compileLet' tmpVar atom name body = do
  let callDirect = canBeCalledDirectly atom
  rTmp <- getReg tmpVar
  when callDirect $ addDirectCallReg rTmp
  comp1 <- compileAtom rTmp atom name False
  comp2 <- compileExpr body
  return $ comp1 ++ comp2

-- This determines whether we'll use Tac_call or Tac_call_cl later
canBeCalledDirectly atom = case atom of
  NVar (NConstantFreeVar _) -> True -- TODO we need a function tag for this case
  NLambda ([]) _ _ -> True
  _ -> False

{-
isConstant atom = case atom of
  NLambda [] _ _ -> True
  NNumber _ -> True
  NPlainSymbol _ -> True
  -- NCompoundSymbol _ False _ -> True
  _ -> False
-}

compileClosure reg freeVars params expr = do
  funAddr <- compileFunc freeVars params expr
  argInstrs <- mapM (uncurry compileSetArgN) $ zipWithIndex freeVars
  -- TODO use the next free register instead of hardcoded value
  let makeClosureInstr = [Tac_load_f 31 funAddr, Tac_make_cl reg 31 (length freeVars)]
  return $ argInstrs ++ makeClosureInstr



compileSetArg var arg = do
  rVar <- getReg var
  return $ Tac_set_arg arg rVar 0

compileSetArgN name arg = do
  rVar <- getRegByName name
  return $ Tac_set_arg arg rVar 0


zipWithIndex l = zip l [0..(length l)]


----- State -----

-- TODO introduce a proper symtable
data CompState = CompState {
                   instructions :: Seq.Seq [Tac Reg]
                 , dataTable :: ConstTable -- rename ConstTable to DataTable
                 , scopes :: [CompScope]
                 }

emptyCompState = CompState {
                   instructions = Seq.fromList []
                 , dataTable = []
                 , scopes = []
                 }

data CompScope = CompScope {
                   functionParams :: Map.Map String Int
                 , freeVariables :: Map.Map String Int

                 -- these are all the registers that hold function values which
                 -- can be called directly with Tac_call. Everything else is 
                 -- called with Tac_call_cl
                 , directCallRegs :: [Int]
                 , codeConstants :: Map.Map String CompCodeConst
                 }

makeScope fps freeVars = CompScope {
               functionParams = fps
             , freeVariables = freeVars
             , directCallRegs = []
             , codeConstants = Map.empty
             }

data CompCodeConst =
    CConstNumber VMWord
  | CConstPlainSymbol SymId
 -- | CConstCompoundSymbol ConstAddr
  | CConstLambda FunAddr


beginFunction freeVars params = do
  state <- get
  let localFreeVars = Map.fromList (zipWithIndex freeVars)
  let paramBindings = Map.fromList (zipWithIndex params)
  let newScope = makeScope paramBindings localFreeVars
  put $ state { scopes = newScope : (scopes state) }
  addr <- addPlaceholderFunction
  return addr

endFunction funAddr code = do
  state <- get
  let instrs = instructions state
  let instrs' = Seq.update funAddr code instrs
  put $ state { scopes = (tail $ scopes state),
                instructions = instrs' }

numParameters :: State CompState Int
numParameters = do
  localParams <- gets $ functionParams.head.scopes
  return $ Map.size localParams

param :: String -> State CompState (Maybe Int)
param name = do
  localParams <- gets $ functionParams.head.scopes
  let res = Map.lookup name localParams
  return res


numFreeVars :: State CompState Int
numFreeVars = do
  localFreeVars <- gets $ freeVariables.head.scopes
  return $ Map.size localFreeVars

freeVar :: String -> State CompState (Maybe Int)
freeVar name = do
  localFreeVars <- gets $ freeVariables.head.scopes
  let res = Map.lookup name localFreeVars
  return res


addPlaceholderFunction = do
  state <- get
  let instrs = instructions state
  let nextFunAddr = Seq.length instrs
  let instrs' = instrs Seq.|> []
  put $ state { instructions = instrs' }
  return nextFunAddr

getRegByName :: String -> State CompState Int
getRegByName name = do
  lookup <- getRegN name
  -- TODO make this less ugly
  -- TODO code duplication, handling of register offsets for free vars should be handled in one place only
  case lookup of
    Just index -> return index
    Nothing -> error $ "Unknown identifier " ++ name
  where getRegN name = do
          p <- param name
          case p of
                  Just i -> return $ Just i
                  Nothing -> do
                          f <- freeVar name
                          numParams <- numParameters
                          return $ (+) <$> f <*> Just numParams

getReg :: NormVar -> State CompState Int
getReg (NConstantFreeVar _) = error "Compiler error"

getReg (NFunParam name) = do
  lookup <- param name
  case lookup of
          Just index -> return index
          Nothing -> error $ "Unknown parameter: " ++ name

-- When calling a closure, the first n registers are formal arguments
-- and the next m registers are closed-over variables
-- TODO document this fact somewhere visible
getReg (NDynamicFreeVar name) = do
  numParams <- numParameters
  lookup <- freeVar name
  case lookup of
          Just index -> return $ numParams + index
          Nothing -> error $ "Unknown free var: " ++ name

getReg (NLocalVar tmpVar name) = do
  numFree <- numFreeVars
  numParams <- numParameters
  return $ numFree + numParams + tmpVar

isDirectCallReg reg = do
  scope <- getScope
  let dCallRegs = directCallRegs scope
  return $ Prelude.elem reg dCallRegs

addDirectCallReg reg = do
  scope <- getScope
  let dCallRegs = directCallRegs scope
  let dCallRegs' = reg : dCallRegs
  putScope $ scope { directCallRegs = dCallRegs' }

getScope = do
  state <- get
  return $ head (scopes state)

putScope s = do
  state <- get
  put $ state { scopes = s : (tail $ scopes state) }


addCodeConst "" _ = return ()
addCodeConst name c = do
  scope <- getScope
  let consts = codeConstants scope
  let consts' = Map.insert name c consts
  putScope $ scope { codeConstants = consts' }

-- This retrieves values for NConstantFreeVar. Those are never inside the current scope
getCodeConstInOuterScope :: String -> State CompState CompCodeConst
getCodeConstInOuterScope name = do
  scps <- gets scopes
  getCodeConst' name scps
  where
    getCodeConst' name [] = error $ "Compiler error: no constant named " ++ name
    getCodeConst' name scps = do
      let consts = codeConstants $ head scps
      case Map.lookup name consts of
        Just c -> return c
        Nothing -> getCodeConst' name $ tail scps


