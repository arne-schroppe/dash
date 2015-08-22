module Language.Dash.API
( run
, runWithPreamble
-- , toNorm
--, toAsm
-- , toAtomicConstants
--, toParsed
) where

import           Language.Dash.Asm.Assembler
import           Language.Dash.CodeGen.BuiltInDefinitions  (preamble)
import           Language.Dash.CodeGen.CodeGen
import           Language.Dash.Internal.Error              (CompilationError (..))
import           Language.Dash.IR.Data
import           Language.Dash.Normalization.Normalization
import           Language.Dash.Parser.Lexer
import           Language.Dash.Parser.Parser
import           Language.Dash.VM.DataEncoding
import           Language.Dash.VM.VM
import           Language.Dash.VM.Types
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

-- Debugging functions

{-

toParsed :: (ExceptT m) => String -> m Expr
toParsed prog = prog |> lex |> parse

toNorm :: => String -> (ExceptT CompilationError m) NstExpr
toNorm prog =
  let (nExpr, _, _) = prog |> lex |> parse |> normalize in
  nExpr


toAsm :: (ExceptT m) => String -> m [[Opcode]]
toAsm prog =
  let (asm, _, _) = prog |> lex |> parse |> normalize ||> compile in
  asm

-}

{-
toAtomicConstants :: String -> ([AtomicConstant], Map.Map ConstAddr VMWord)
toAtomicConstants prog =
  let (_, ctable, _) = prog |> lex |> parse |> normalize ||> compile in
  atomizeConstTable ctable
-}


