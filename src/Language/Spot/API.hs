module Language.Spot.API 
(
  run
, toNorm
, toAsm
, toAtomicConstants
) where

import           Prelude                              hiding (lex)
import           Language.Spot.Parser.Lexer
import           Language.Spot.Parser.Parser
import           Language.Spot.CodeGen.Normalization
import           Language.Spot.CodeGen.CodeGen
import           Language.Spot.VM.Assembler
import           Language.Spot.VM.Types
import           Language.Spot.VM.Bits
import           Language.Spot.VM.VM
import           Language.Spot.IR.Tac
import           Language.Spot.IR.Norm

import           Data.Word
import qualified Data.IntMap                          as IntMap

(|>) = flip ($)

(||>) (a, b, c) d = d a b c

-- TODO have separate compile and run and add methods (add is for repl)
-- TODO in cabal only export API module and reexport all relevant types and functions here (or rename this module)

-- TODO add showAsm
-- also, optimize the const table (remove duplicates)
-- don't bleed Word32 (and fromIntegral) out into the rest. Type Consttable as [[Int]] or [ConstTableEntry] where ConstTableEntry = [Int]
-- Add better typing for several things, also for uncompiled asm

-- TODO find a style and use it consistently
-- TODO explain what each module does



toNorm :: String -> NormExpr
toNorm prog =
  let (nExpr, ctable, symNames) = prog |> lex |> parse |> normalize in
  nExpr

toAsm :: String -> [[Tac]]
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

