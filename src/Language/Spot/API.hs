module Language.Spot.API where

import Language.Spot.Parser.Lexer as L
import Language.Spot.Parser.Parser
import Language.Spot.CodeGen.CodeGen
import Language.Spot.VM.OpcodeAsm
import Language.Spot.VM.Types
import Language.Spot.VM.Bits
import Language.Spot.VM.VM

(|>) = flip ($)

(||>) a b = a |> (uncurry b)

-- TODO have separate compile and run and add methods (add is for repl)

run :: String -> IO VMValue
run prog = do
  result <- prog |> L.lex |> parse |> generateCode ||> assemble ||> execute
  return $ decode result

