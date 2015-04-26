module Language.Spot.API where

import Prelude hiding (lex)
import Language.Spot.Parser.Lexer
import Language.Spot.Parser.Parser
import Language.Spot.CodeGen.AstToAnf
import Language.Spot.CodeGen.CodeGen2
import Language.Spot.VM.Assembler
import Language.Spot.VM.Types
import Language.Spot.VM.Bits
import Language.Spot.VM.VM
import Language.Spot.IR.Tac
import Language.Spot.IR.Anf

import Data.Word
import qualified Data.IntMap as IntMap

(|>) = flip ($)

(||>) (a, b, c) d = d a b c

-- TODO have separate compile and run and add methods (add is for repl)
-- TODO in cabal only export API module and reexport all relevant types and functions here (or rename this module)

-- TODO add showAsm
-- also, optimize the const table (remove duplicates)
-- don't bleed Word32 (and fromIntegral) out into the rest. Type Consttable as [[Int]] or [ConstTableEntry] where ConstTableEntry = [Int]
-- Add better typing for several things, also for uncompiled asm

toAsm :: String -> [[Tac Reg]]
toAsm prog =
  let (asm, ctable, symNames) = prog |> lex |> parse |> normalize ||> compile in
  asm

toAtomicConstants :: String -> ([AtomicConstant], IntMap.IntMap VMWord)
toAtomicConstants prog =
  let (asm, ctable, symNames) = prog |> lex |> parse |> normalize ||> compile in
  atomizeConstTable ctable


run :: String -> IO VMValue
run prog = do
  (value, ctable, symNames) <-  prog |> lex |> parse |> normalize ||> compile ||> assemble ||> execute
  return $ decode value ctable symNames

