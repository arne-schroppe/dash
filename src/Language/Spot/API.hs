module Language.Spot.API where

import Prelude hiding (lex)
import Language.Spot.Parser.Lexer
import Language.Spot.Parser.Parser
import Language.Spot.CodeGen.CodeGen
import Language.Spot.VM.Assembler
import Language.Spot.VM.Types
import Language.Spot.VM.Bits
import Language.Spot.VM.VM
import Language.Spot.IR.Tac

import Data.Word
import qualified Data.IntMap as IntMap

(|>) = flip ($)

(||>) (a, b, c) d = d a b c

-- TODO have separate compile and run and add methods (add is for repl)
-- TODO in cabal only export API module and reexport all relevant types and functions here (or rename this module)

-- TODO add showConstTable that translates a const table to something readable
-- TODO or better: Store const table in an intermediate format first! (this way we could even add other backends)
-- TODO add showAsm
-- also, optimize the const table (remove duplicates)
-- don't bleed Word32 (and fromIntegral) out into the rest. Type Consttable as [[Int]] or [ConstTableEntry] where ConstTableEntry = [Int]
-- Add better typing for several things, also for uncompiled asm

toAsm :: String -> [[Tac]]
toAsm prog =
  let (asm, ctable, symNames) = prog |> lex |> parse |> compile in
  asm

toRawCTable :: String -> ([AtomicConstant], IntMap.IntMap VMWord)
toRawCTable prog =
  let (asm, ctable, symNames) = prog |> lex |> parse |> compile in
  encodeConstTableToAtomicConsts ctable


run :: String -> IO VMValue
run prog = do
  (value, ctable, symNames) <-  prog |> lex |> parse |> compile ||> assemble ||> execute
  return $ decode value ctable symNames

