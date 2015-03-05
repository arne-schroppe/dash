module Language.Spot.API where

import Language.Spot.Parser.Lexer as L
import Language.Spot.Parser.Parser
import Language.Spot.CodeGen.CodeGen
import Language.Spot.VM.OpcodeAsm
import Language.Spot.VM.VMBits
import Language.Spot.VM.VM

(|>) = flip ($)


run :: String -> IO VMValue
run prog = do
  result <- prog |> L.lex |> parse |> generateCode |> (uncurry assemble) |> (uncurry execute)
  return $ decode result

