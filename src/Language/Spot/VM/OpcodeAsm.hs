module Language.Spot.VM.OpcodeAsm where

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


combineFunctions :: [[Opcode]] -> ([Opcode], [Int])
combineFunctions funcs = (fst combined, reverse $ snd combined)
  where combined = foldl calcFuncAddr ([], []) funcs
        calcFuncAddr acc l = ( fst acc ++ l, length (fst acc) : snd acc )


compileOpcode :: [Int] -> Opcode -> Word32
compileOpcode funcAddrs opc =
  case opc of
    Op_halt -> 0
    Op_load_i r0 i -> instructionRI 1 r0 i
    _ -> error "Unknown opcode"


instSize = 32

instructionRI opcId register value =
   (opcId `shiftL` (instSize - 4)) .|. (register `shiftL` (instSize - (4 + 5))) .|. value

{-
instructionRRR opcn r0 r1 r2 =
  let instr = (opcn lsl (isize - 4))
      lor (r0 lsl (isize - (4 + 5)))
      lor (r1 lsl (isize - (4 + 2 * 5)))
      lor (r2 lsl (isize - (4 + 3 * 5)))
  in
  int32_of_int instr
-}



