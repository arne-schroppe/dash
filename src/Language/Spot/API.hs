module Language.Spot.API where

import Prelude hiding (lex)
import Language.Spot.Parser.Lexer
import Language.Spot.Parser.Parser
import Language.Spot.CodeGen.CodeGen
import Language.Spot.VM.OpcodeAsm
import Language.Spot.VM.Types
import Language.Spot.VM.Bits
import Language.Spot.VM.VM
import Language.Spot.IR.Opcode

(|>) = flip ($)

(||>) (a, b, c) d = d a b c

-- TODO have separate compile and run and add methods (add is for repl)
-- TODO in cabal only export API module and reexport all relevant types and functions here (or rename this module)


toAsm :: String -> [[Opcode]]
toAsm prog =
  let (asm, ctable, symNames) = prog |> lex |> parse |> compile in
  asm


run :: String -> IO VMValue
run prog = do
  (value, ctable, symNames) <- prog |> lex |> parse |> compile ||> assemble ||> execute
  return $ decode value ctable symNames

