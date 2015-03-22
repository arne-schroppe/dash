module Language.Spot.VM.OpcodeAsm (
  assemble

) where

-- Translates [[Opcode]] to data for the virtual machine

import Data.Word
import Data.List
import Data.Bits
import Language.Spot.IR.Opcode
import Language.Spot.VM.Types


assemble :: [[Opcode]] -> ConstTable -> SymbolNameList -> ([VMWord], ConstTable, SymbolNameList)
assemble funcs ctable symnames =
  (map (compileOpcode funcAddrs) opcodes, ctable, symnames)
  where opcodes = fst combined
        funcAddrs = snd combined
        combined = combineFunctions funcs


combineFunctions :: [[Opcode]] -> ([Opcode], [VMWord])
combineFunctions funcs = (fst combined, reverse $ snd combined)
  where combined = foldl calcFuncAddr ([], []) funcs
        calcFuncAddr acc funcOpcodes =
          let allOpcodes = fst acc in
          let funcAddrs = snd acc in
          ( allOpcodes ++ funcOpcodes, (fromIntegral $ length allOpcodes) : funcAddrs )


compileOpcode :: [VMWord] -> Opcode -> Word32
compileOpcode funcAddrs opc =
  let r = fromIntegral in
  let i = fromIntegral in
  case opc of
    Op_ret             -> instructionRI   0 0 0
    Op_load_i r0 i     -> instructionRI   1 (r r0) i
    Op_load_f r0 fi    -> instructionRI   1 (r r0) (funcAddrs !! fi)
    Op_load_s r0 n     -> instructionRI   2 (r r0) n
    Op_load_sd r0 n    -> instructionRI   3 (r r0) n
    Op_load_c r0 n     -> instructionRI   4 (r r0) n
    Op_add r0 r1 r2    -> instructionRRR  5 (r r0) (r r1) (r r2)
    Op_sub r0 r1 r2    -> instructionRRR  6 (r r0) (r r1) (r r2)
    Op_move r0 r1      -> instructionRRR  7 (r r0) (r r1) (i 0)
    Op_call r0 fr n    -> instructionRRR  8 (r r0) (r fr) (i n)
    Op_call_cl r0 fr n -> instructionRRR  9 (r r0) (r fr) (i n)
    Op_make_cl r0 fr n -> instructionRRR 10 (r r0) (r fr) (i n)
    Op_jmp n           -> instructionRI  11 0 (i n)
    Op_match r0 r1 r2  -> instructionRRR 12 (r r0) (r r1) (r r2)



instSize = 32

instructionRI opcId register value =
  (opcId `shiftL` (instSize - 4)) .|. (register `shiftL` (instSize - (4 + 5))) .|. value

instructionRRR opcId r0 r1 r2 =
  (opcId `shiftL` (instSize - 4))
      .|. (r0 `shiftL` (instSize - (4 + 5)))
      .|. (r1 `shiftL` (instSize - (4 + 2 * 5)))
      .|. (r2 `shiftL` (instSize - (4 + 3 * 5)))



