module Language.Dash.API
( run
, runWithPreamble
) where

import           Language.Dash.Asm.Assembler
import           Language.Dash.CodeGen.BuiltInDefinitions  (preamble)
import           Language.Dash.CodeGen.CodeGen
import           Language.Dash.Error.Error                 (CompilationError (..))
import           Language.Dash.IR.Data
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
  let compiledOrError = compileProgram prog
  case compiledOrError of
    Left err -> return (Left err)
    Right (encodedProgram, encodedConstTable, symNames) -> do
            (value, constTable, symNames') <- execute encodedProgram encodedConstTable symNames
            decoded <- decode value constTable symNames'
            return $ Right decoded


compileProgram :: String -> Either CompilationError ([VMWord], [VMWord], SymbolNameList)
compileProgram prog = do
  let lexed = lex prog
  let ast = parse lexed
  (normExpr, constTable, symNames) <- normalize ast
  (opcodes, constTable', symNames') <- compile normExpr constTable symNames
  (encodedProgram, encodedConstTable) <- assemble opcodes constTable'
  return (encodedProgram, encodedConstTable, symNames')

