module Language.Spot.Asm.Assembler (
  assemble
, assembleWithEncodedConstTable
) where


import           Data.Bits
import           Data.Word
import           Language.Spot.IR.Data
import           Language.Spot.IR.Tac
import           Language.Spot.VM.Types
import           Language.Spot.Asm.DataAssembler


{-

Assembler
~~~~~~~~~

The assembler takes lists of three address codes and generates the actual byte code for
the virtual machine. It also encodes all static objects used at runtime (the const table).

The code generator stores each function as a list so that the input type for the assembler
is [[tac]]. Function addresses in the input code are indices of the outer list. They are
turned into real addresses by the assembler.

-}




-- TODO do we encode nested symbols depth-first or breadth-first? Try both and measure performance!


assemble :: [[Tac]] -> ConstTable -> SymbolNameList -> ([VMWord], [VMWord], SymbolNameList)
assemble funcs ctable symnames =
  let (consts, addrConvert) = encodeConstTable ctable in
  assembleWithEncodedConstTable funcs consts addrConvert symnames


assembleWithEncodedConstTable :: [[Tac]] -> [VMWord] -> (Int -> VMWord) -> SymbolNameList -> ([VMWord], [VMWord], SymbolNameList)
assembleWithEncodedConstTable funcs encCTable cAddrConverter symnames =
  (map (assembleTac funcAddrs cAddrConverter) instructions, encCTable, symnames)
  where instructions = fst combined
        funcAddrs = snd combined
        combined = combineFunctions funcs


combineFunctions :: [[Tac]] -> ([Tac], [VMWord])
combineFunctions funcs = (fst combined, reverse $ snd combined)
  where combined = foldl calcFuncAddr ([], []) funcs
        calcFuncAddr acc funcInstrs =
          let allInstrs = fst acc in
          let funcAddrs = snd acc in
          ( allInstrs ++ funcInstrs, (fromIntegral $ length allInstrs) : funcAddrs )


assembleTac :: [VMWord] -> (Int -> VMWord) -> Tac -> Word32
assembleTac funcAddrs addrConv opc =
  let r = fromIntegral in
  let i = fromIntegral in
  case opc of
    Tac_ret r0              -> instructionRI   0 (r r0) 0
    Tac_load_i r0 n         -> instructionRI   1 (r r0) n
    Tac_load_addr r0 a      -> instructionRI   1 (r r0) (addrConv a)
    Tac_load_f r0 fi        -> instructionRI   1 (r r0) (funcAddrs !! fi)
    Tac_load_ps r0 s        -> instructionRI   2 (r r0) (i s)
    Tac_load_cs r0 a        -> instructionRI   3 (r r0) (addrConv a)
    Tac_load_c r0 a         -> instructionRI   4 (r r0) (addrConv a)
    Tac_add r0 r1 r2        -> instructionRRR  5 (r r0) (r r1) (r r2)
    Tac_sub r0 r1 r2        -> instructionRRR  6 (r r0) (r r1) (r r2)
    Tac_move r0 r1          -> instructionRRR  7 (r r0) (r r1) (i 0)
    Tac_call r0 fr n        -> instructionRRR  8 (r r0) (r fr) (i n)
    Tac_call_cl r0 fr n     -> instructionRRR  9 (r r0) (r fr) (i n)
    Tac_make_cl r0 fr n     -> instructionRRR 10 (r r0) (r fr) (i n)
    Tac_jmp n               -> instructionRI  11 0 (i n)
    Tac_match r0 r1 r2      -> instructionRRR 12 (r r0) (r r1) (r r2)
    Tac_set_arg arg r1 n    -> instructionRRR 13 (i arg) (r r1) (i n)
    Tac_tail_call fr n      -> instructionRRR 14 (i 0) (r fr) (i n)
    Tac_tail_call_cl fr n   -> instructionRRR 15 (i 0) (r fr) (i n)
    Tac_set_cl_val clr r1 n -> instructionRRR 16 (r clr) (r r1) (i n)



instBits, opcBits, regBits :: Int
instBits = 32
opcBits = 6
regBits = 5


-- an instruction containing a register and a number
instructionRI :: VMWord -> VMWord -> VMWord -> VMWord
instructionRI opcId register value =
  (opcId `shiftL` (instBits - opcBits)) .|. (register `shiftL` (instBits - (opcBits + regBits))) .|. value

-- an instruction containing three registers
instructionRRR :: VMWord -> VMWord -> VMWord -> VMWord -> VMWord
instructionRRR opcId r0 r1 r2 =
   (opcId `shiftL` (instBits - opcBits))
  .|. (r0 `shiftL` (instBits - (opcBits + regBits)))
  .|. (r1 `shiftL` (instBits - (opcBits + 2 * regBits)))
  .|. (r2 `shiftL` (instBits - (opcBits + 3 * regBits)))




