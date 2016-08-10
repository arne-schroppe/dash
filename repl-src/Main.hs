import           Control.Monad.Trans                      (liftIO)
import           Language.Dash.API
import           Language.Dash.BuiltIn.BuiltInDefinitions (errorSymbolName, runtimeErrorSymbolName)
import           Language.Dash.Error.Error
import           Language.Dash.IR.Ast                     (Expr (..))
import           Language.Dash.IR.Data                    (SymbolNameList)
import           Language.Dash.IR.Opcode                  (EncodedFunction(..))
import           Language.Dash.VM.Types
import           System.Console.Haskeline
import           System.Environment
import           System.IO

data ReplState = ReplState {
  rsProg           :: Expr,
  rsMultilineMode  :: Bool,
  rsMultilineInput :: String
}

main = do
  putStrLn "Welcome to the dash repl\nType \".quit\" to quit\nUse \"...\" to toggle multi line input"
  let prog0 = either (error . show) id $ parseWithPreamble ":true" -- obtain preamble
  -- let prog0 = either (error . show) id $ parseProgram ":true"
  runInputT (setComplete noCompletion defaultSettings) $ loop $ ReplState prog0 False ""



-- TODO add commands ".rewrite" (to change function implementations, etc), ".save" to save to a file
-- Also store session on end and make it possible to resume it on next start
-- Add ".import" command once we have that functionality
loop :: ReplState -> InputT IO ()
loop state = do
  minput <- getInputLine $ if rsMultilineMode state then "… " else "> "
  case minput of
    Nothing -> exit
    Just ".quit" -> exit
    Just ".exit" -> exit
    Just "..."   -> if rsMultilineMode state
                      then do prog' <- eval (rsMultilineInput state) (rsProg state)
                              loop $ state { rsMultilineMode = False, rsMultilineInput = "", rsProg = prog' }
                      else loop $ state { rsMultilineMode = True }
    Just ".show_compiled" -> do dumpOpcodes (rsProg state); loop state
    Just ""      -> loop state
    Just input -> do state' <- if rsMultilineMode state
                               then return $ state { rsMultilineInput = (rsMultilineInput state) ++ "\n" ++ input }
                               else do prog' <- eval input (rsProg state)
                                       return $ state { rsProg = prog' }
                     loop state'

    where
      exit = outputStrLn "bye."

eval :: String -> Expr -> InputT IO Expr
eval input existingProg = do
  let parseResult = parseProgram input
  case parseResult of
    Left err -> outputStrLn (show err) >> return existingProg
    Right newProg -> do
      let combinedProg = appendExpr newProg existingProg
      result <- liftIO (runExpr combinedProg)
      case result of
        Left err -> do outputStrLn $ show err
                       return existingProg
        Right value@(VMSymbol errorSymbolName [VMSymbol runtimeErrorSymbolName [], _]) ->
                    do outputStrLn $ show value; return existingProg
        Right value -> do outputStrLn $ show value; return combinedProg

dumpOpcodes :: Expr -> InputT IO ()
dumpOpcodes prog = do
  let compResult = compileExpr prog
  case compResult of
    Left err -> outputStrLn "Compilation error"
    Right (compiled, constTable, symNames) -> do outputStrLn "--- opcodes ---"
                                                 outputStrLn $ showOpcodes compiled

                                                 outputStrLn "\n--- const table ---"
                                                 outputStrLn $ showNewlineSeparated constTable

                                                 outputStrLn "\n--- symbol names ---"
                                                 outputStrLn $ showSymbolNames symNames

showOpcodes :: [EncodedFunction] -> String
showOpcodes funs =
  let stringified = map showEncodedFunction funs in
  foldl (\a b -> a ++ "\n" ++ b) "" stringified

showEncodedFunction :: EncodedFunction -> String
showEncodedFunction fun =
  let opcodes = cfOpcodes fun in
  let stringified = map show opcodes in
  foldl (\a b -> a ++ "\n" ++ b) "" stringified

showNewlineSeparated :: Show a => [a] -> String
showNewlineSeparated s =
  let stringified = map show s in
  foldl (\a b -> a ++ "\n" ++ b) "" stringified


showSymbolNames :: SymbolNameList -> String
showSymbolNames symNames =
  snd $ foldl (\(index, acc) s -> (index + 1, acc ++ "\n" ++ show index ++ ": " ++ s)) (0, "") symNames

appendExpr :: Expr -> Expr -> Expr
appendExpr newExpr existingExpr =
  case existingExpr of
    LocalBinding b e -> LocalBinding b $ appendExpr newExpr e
    DestructuringBind pat boundExpr e -> DestructuringBind pat boundExpr $ appendExpr newExpr e
    _ -> newExpr


