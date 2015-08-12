module Language.Dash.API
( run
, runWithPreamble
, toNorm
, toAsm
, toAtomicConstants
) where

import qualified Data.Map                                  as Map
import           Language.Dash.Asm.Assembler
import           Language.Dash.Asm.DataAssembler
import           Language.Dash.CodeGen.BuiltInDefinitions (preamble)
import           Language.Dash.CodeGen.CodeGen
import           Language.Dash.IR.Data
import           Language.Dash.IR.Nst
import           Language.Dash.IR.Opcode
import           Language.Dash.Normalization.Normalization
import           Language.Dash.Parser.Lexer
import           Language.Dash.Parser.Parser
import           Language.Dash.VM.DataEncoding
import           Language.Dash.VM.Types
import           Language.Dash.VM.VM
import           Prelude                                   hiding (lex)

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

(||>) :: (t1, t2, t3) -> (t1 -> t2 -> t3 -> a) -> a
(||>) (a, b, c) d = d a b c

-- TODO have separate compile and run and add methods (add is for repl)
-- TODO in cabal only export API module and reexport all relevant types and functions
-- here (or rename this module)

-- TODO add showAsm
-- also, optimize the const table (remove duplicates)
-- don't bleed Word32 (and fromIntegral) out into the rest. Type Consttable as [[Int]]
-- or [ConstTableEntry] where ConstTableEntry = [Int]
-- Add better typing for several things, also for uncompiled asm

-- TODO find a style and use it consistently
-- TODO explain what each module does

-- TODO don't call this "API", unless it re-exports functions and types

runWithPreamble :: String -> IO VMValue
runWithPreamble prog =
  let prog' = preamble ++ prog in
  run prog'

run :: String -> IO VMValue
run prog = do
  (value, ctable, symNames) <- prog
                               |> lex
                               |> parse
                               |> normalize
                               ||> compile
                               ||> assemble
                               ||> execute
  decode value ctable symNames


-- Debugging functions

toNorm :: String -> NstExpr
toNorm prog =
  let (nExpr, _, _) = prog |> lex |> parse |> normalize in
  nExpr


toAsm :: String -> [[Opcode]]
toAsm prog =
  let (asm, _, _) = prog |> lex |> parse |> normalize ||> compile in
  asm


toAtomicConstants :: String -> ([AtomicConstant], Map.Map ConstAddr VMWord)
toAtomicConstants prog =
  let (_, ctable, _) = prog |> lex |> parse |> normalize ||> compile in
  atomizeConstTable ctable


