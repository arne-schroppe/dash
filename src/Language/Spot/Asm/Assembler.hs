module Language.Spot.Asm.Assembler (
  assemble
, assembleWithEncodedConstTable
) where

-- TODO put this into its own namespace

-- Translates [[Tac]] to data for the virtual machine

import           Control.Monad.State
import           Data.Bits
import           Data.List
import           Data.Maybe
import           Data.Word
import           Language.Spot.IR.Data
import           Language.Spot.IR.Tac
import           Language.Spot.VM.Bits
import           Language.Spot.VM.Types
import           Language.Spot.Asm.DataAssembler

import           Debug.Trace

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
    Tac_ret r0          -> instructionRI   0 (r r0) 0
    Tac_load_i r0 i     -> instructionRI   1 (r r0) i
    Tac_load_addr r0 a  -> instructionRI   1 (r r0) (addrConv a)
    Tac_load_f r0 fi    -> instructionRI   1 (r r0) (funcAddrs !! fi)
    Tac_load_ps r0 s    -> instructionRI   2 (r r0) (i s)
    Tac_load_cs r0 a    -> instructionRI   3 (r r0) (addrConv a)
    Tac_load_c r0 a     -> instructionRI   4 (r r0) (addrConv a)
    Tac_add r0 r1 r2    -> instructionRRR  5 (r r0) (r r1) (r r2)
    Tac_sub r0 r1 r2    -> instructionRRR  6 (r r0) (r r1) (r r2)
    Tac_move r0 r1      -> instructionRRR  7 (r r0) (r r1) (i 0)
    Tac_call r0 fr n    -> instructionRRR  8 (r r0) (r fr) (i n)
    Tac_call_cl r0 fr n -> instructionRRR  9 (r r0) (r fr) (i n)
    Tac_make_cl r0 fr n -> instructionRRR 10 (r r0) (r fr) (i n)
    Tac_jmp n           -> instructionRI  11 0 (i n)
    Tac_match r0 r1 r2  -> instructionRRR 12 (r r0) (r r1) (r r2)
    Tac_set_arg arg r1 n  -> instructionRRR 13 (i arg) (r r1) (i n)
    Tac_tail_call fr n    -> instructionRRR 14 (i 0) (r fr) (i n)
    Tac_tail_call_cl fr n -> instructionRRR 15 (i 0) (r fr) (i n)
    Tac_set_cl_arg clr r1 n -> instructionRRR 16 (r clr) (r r1) (i n)



instBits = 32
opcBits = 6
regBits = 5


instructionRI :: VMWord -> VMWord -> VMWord -> VMWord
instructionRI opcId register value =
  (opcId `shiftL` (instBits - opcBits)) .|. (register `shiftL` (instBits - (opcBits + regBits))) .|. value


instructionRRR :: VMWord -> VMWord -> VMWord -> VMWord -> VMWord
instructionRRR opcId r0 r1 r2 =
   (opcId `shiftL` (instBits - opcBits))
  .|. (r0 `shiftL` (instBits - (opcBits + regBits)))
  .|. (r1 `shiftL` (instBits - (opcBits + 2 * regBits)))
  .|. (r2 `shiftL` (instBits - (opcBits + 3 * regBits)))




