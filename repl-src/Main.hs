import           Control.Monad.Trans (liftIO)
import           Language.Dash.API
import           Language.Dash.IR.Ast (Expr(..))
import           System.Console.Haskeline
import           System.Environment
import           System.IO

main =
  let prog0 = parseWithPreamble ":true" in -- obtain preamble
  runInputT defaultSettings $ loop prog0
  where
    loop :: Expr -> InputT IO ()
    loop prog = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> return ()
        Just ":quit" -> return ()
        Just ":exit" -> return ()
        Just input -> do prog' <- eval input prog
                         loop prog'

eval :: String -> Expr -> InputT IO Expr
eval input existingProg = do
  let newProg = parseProgram input
  let combinedProg = appendExpr newProg existingProg
  result <- liftIO (runExpr combinedProg)
  case result of
    Left err -> do outputStrLn $ show err
                   return existingProg
    Right value -> do outputStrLn $ show value; return combinedProg


appendExpr :: Expr -> Expr -> Expr
appendExpr newExpr existingExpr =
  case existingExpr of
    LocalBinding b e -> LocalBinding b $ appendExpr newExpr e
    _ -> newExpr


