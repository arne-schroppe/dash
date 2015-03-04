module Language.Spot.VM.OpcodeAsm (
  assemble

) where

-- Translates [[Opcode]] to data for the virtual machine

import Data.Word
import Data.List
import Data.Bits
import Language.Spot.IR.Opcode


assemble :: [[Opcode]] -> [Word32]
assemble funcs = map (compileOpcode funcAddrs) opcodes
  where opcodes = fst combined
        funcAddrs = snd combined
        combined = combineFunctions funcs


combineFunctions :: [[Opcode]] -> ([Opcode], [Word32])
combineFunctions funcs = (fst combined, reverse $ snd combined)
  where combined = foldl calcFuncAddr ([], []) funcs
        calcFuncAddr acc l = ( fst acc ++ l, (fromIntegral $ length (fst acc)) : snd acc )


compileOpcode :: [Word32] -> Opcode -> Word32
compileOpcode funcAddrs opc =
  case opc of
    Op_halt            -> instructionRI   0 0 0
    Op_load_i r0 i     -> instructionRI   1 r0 i
    Op_load_f r0 fi    -> instructionRI   1 r0 (funcAddrs !! fi)
    Op_load_s r0 n     -> instructionRI   2 r0 n
    Op_load_sd r0 n    -> instructionRI   3 r0 n
    Op_load_c r0 n     -> instructionRI   4 r0 n
    Op_add r0 r1 r2    -> instructionRRR  5 r0 r1 r2
    Op_sub r0 r1 r2    -> instructionRRR  6 r0 r1 r2
    Op_move r0 r1      -> instructionRRR  7 r0 r1 0
    Op_call r0 fr n    -> instructionRRR  8 r0 fr n
    Op_call_cl r0 fr n -> instructionRRR  9 r0 fr n
    Op_ret             -> instructionRI  10 0 0
    Op_make_cl r0 fr n -> instructionRRR 11 r0 fr n
    Op_jmp n           -> instructionRI  12 0 n
    Op_match r0 r1 r2  -> instructionRRR 13 r0 r1 r2



instSize = 32

instructionRI opcId register value =
  (opcId `shiftL` (instSize - 4)) .|. (register `shiftL` (instSize - (4 + 5))) .|. value

instructionRRR opcId r0 r1 r2 =
  (opcId `shiftL` (instSize - 4))
      .|. (r0 `shiftL` (instSize - (4 + 5)))
      .|. (r1 `shiftL` (instSize - (4 + 2 * 5)))
      .|. (r2 `shiftL` (instSize - (4 + 3 * 5)))



