module Language.Dash.API
( run
, runExpr
, runWithPreamble
, normalizeProgram
, parseProgram
, assembleProgram
, compileExpr
, parseWithPreamble
, showNormalizedProgram
, showCompiledProgram
) where

import           Language.Dash.Asm.Assembler
import           Language.Dash.BuiltIn.BuiltInDefinitions  (preamble)
import           Language.Dash.CodeGen.CodeGen
import           Language.Dash.Error.Error                 (CompilationError (..))
import           Language.Dash.IR.Ast                      (Expr)
import           Language.Dash.IR.Data
import           Language.Dash.IR.Opcode
import           Language.Dash.IR.Nst                      (NstExpr)
import           Language.Dash.Normalization.Normalization
import           Language.Dash.Parser.Lexer
import           Language.Dash.Parser.Parser
import           Language.Dash.VM.DataEncoding
import           Language.Dash.VM.Types
import           Language.Dash.VM.VM
import           Prelude                                   hiding (lex)


-- TODO Add license header everywhere!


runWithPreamble :: String -> IO (Either CompilationError VMValue)
runWithPreamble prog =
  let prog' = preamble ++ prog in
  run prog'

run :: String -> IO (Either CompilationError VMValue)
run prog = do
  let parsed = parseProgram prog
  case parsed of
    Left err -> return $ Left err
    Right expr ->
      runExpr expr

runExpr :: Expr -> IO (Either CompilationError VMValue)
runExpr expr = do
  let compiledOrError = assembleExpr expr
  case compiledOrError of
    Left err -> return (Left err)
    Right (encodedProgram, encodedConstTable, symNames) -> do
            (value, constTable, symNames') <- execute encodedProgram encodedConstTable symNames
            decoded <- decode value constTable symNames'
            return $ Right decoded


assembleProgram :: String -> Either CompilationError ([VMWord], [VMWord], SymbolNameList)
assembleProgram prog = do
  ast <- parseProgram prog
  assembleExpr ast

assembleExpr :: Expr -> Either CompilationError ([VMWord], [VMWord], SymbolNameList)
assembleExpr ast = do
  (opcodes, constTable', symNames') <- compileExpr ast
  (encodedProgram, encodedConstTable) <- assemble opcodes constTable'
  return (encodedProgram, encodedConstTable, symNames')

compileExpr :: Expr -> Either CompilationError ([EncodedFunction], ConstTable, SymbolNameList)
compileExpr ast = do
  (normExpr, constTable, symNames) <- normalize ast
  compile normExpr constTable symNames

normalizeProgram :: String -> Either CompilationError (NstExpr, ConstTable, SymbolNameList)
normalizeProgram prog = do
  lexed <- lex prog
  ast <- parse lexed
  normalize ast


parseWithPreamble :: String -> Either CompilationError Expr
parseWithPreamble prog =
  let prog' = preamble ++ prog in
  parseProgram prog'

parseProgram :: String -> Either CompilationError Expr
parseProgram prog = do
  lexed <- lex prog
  parse lexed

-- for testing
showNormalizedProgram :: String -> String
showNormalizedProgram prog =
  let result = normalizeProgram prog in
  case result of
    Left err -> show err
    Right (nExpr, _, _) -> show nExpr

showCompiledProgram :: String -> String
showCompiledProgram prog =
  let parsed = parseProgram prog in
  case parsed of
    Left err -> show err
    Right p' ->
      let result = compileExpr p' in
      case result of
        Left err -> show err
        Right (compiled, _, _) -> show compiled
