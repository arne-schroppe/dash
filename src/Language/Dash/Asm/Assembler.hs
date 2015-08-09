module Language.Dash.Asm.Assembler (
  assemble
, assembleWithEncodedConstTable
) where


import           Data.Bits
import qualified Data.Sequence                   as Seq
import           Language.Dash.Asm.DataAssembler
import           Language.Dash.IR.Data
import           Language.Dash.IR.Opcode
import           Language.Dash.VM.Types
import           Language.Dash.Constants


{-

Assembler
~~~~~~~~~

The assembler takes lists of three address codes and generates the actual byte code for
the virtual machine. It also encodes all static objects used at runtime (the const table).
The latter is done by the DataAssembler.

The code generator stores each function as a list, so the input type for the assembler
is [[tac]]. Function addresses in the input code are indices of the outer list. They are
turned into real addresses by the assembler.

-}



assemble :: [[Opcode]]
         -> ConstTable
         -> SymbolNameList
         -> ([VMWord], [VMWord], SymbolNameList)
assemble funcs ctable symnames =
  let (consts, addrConvert) = encodeConstTable ctable in
  assembleWithEncodedConstTable funcs consts addrConvert symnames


assembleWithEncodedConstTable :: [[Opcode]]
                              -> [VMWord]
                              -> (ConstAddr
                              -> VMWord)
                              -> SymbolNameList
                              -> ([VMWord], [VMWord], SymbolNameList)
assembleWithEncodedConstTable funcs encCTable constAddrConverter symnames =
  (map assembleOpcode instructions, encCTable, symnames)
  where
    assembleOpcode = assembleTac funcAddrs constAddrConverter
    instructions = fst combined
    funcAddrs = snd combined
    combined = combineFunctions funcs


-- Converts the nested list of functions into a flat list, and additionally provides
-- a map from indices in the nested list to the index in the flat list (that map is
-- just a sequence with the same length as the nested list). The map helps us to find
-- function references in the Tac code in our generated binary code.
combineFunctions :: [[Opcode]] -> ([Opcode], Seq.Seq VMWord)
combineFunctions =
  foldl calcFuncAddr ([], Seq.empty)
  where
    calcFuncAddr acc funcInstrs =
      let allInstrs = fst acc in
      let funcAddrs = snd acc in
      ( allInstrs ++ funcInstrs, funcAddrs Seq.|> fromIntegral (length allInstrs) )


assembleTac :: Seq.Seq VMWord -> (ConstAddr -> VMWord) -> Opcode -> VMWord
assembleTac funcAddrs addrConv opc =
  let r = regToInt in
  let i = fromIntegral in
  let sym = fromIntegral . symIdToInt in
  let caddr a = fromIntegral (addrConv a) in
  let faddr a = fromIntegral $ funcAddrs `Seq.index` funcAddrToInt a in
  case opc of
    OpcRet r0              -> instructionRI   0 (r r0) 0
    OpcLoadI r0 n          -> instructionRI   1 (r r0) (biasNumber n)
    OpcLoadAddr r0 a       -> instructionRI   1 (r r0) (caddr a)
    OpcLoadPS r0 s         -> instructionRI   2 (r r0) (sym s)
    OpcLoadCS r0 a         -> instructionRI   3 (r r0) (caddr a)
    OpcLoadC r0 a          -> instructionRI   4 (r r0) (caddr a)
    OpcLoadF r0 fa         -> instructionRI   5 (r r0) (faddr fa)
    OpcAdd r0 r1 r2        -> instructionRRR  6 (r r0) (r r1) (r r2)
    OpcSub r0 r1 r2        -> instructionRRR  7 (r r0) (r r1) (r r2)
    OpcMul r0 r1 r2        -> instructionRRR  8 (r r0) (r r1) (r r2)
    OpcDiv r0 r1 r2        -> instructionRRR  9 (r r0) (r r1) (r r2)
    OpcMove r0 r1          -> instructionRRR 10 (r r0) (r r1) (i 0)
    OpcCall r0 fr n        -> instructionRRR 11 (r r0) (r fr) (i n)
    OpcGenAp r0 fr n       -> instructionRRR 12 (r r0) (r fr) (i n)
    OpcTailCall fr n       -> instructionRRR 13 (i 0) (r fr) (i n)
    OpcTailGenAp r0 fr n   -> instructionRRR 14 (r r0) (r fr) (i n)
    OpcPartAp r0 fr n      -> instructionRRR 15 (r r0) (r fr) (i n)
    OpcJmp n               -> instructionRI  16 0 (biasNumber n)
    OpcMatch r0 r1 r2      -> instructionRRR 17 (r r0) (r r1) (r r2)
    OpcSetArg arg r1 n     -> instructionRRR 18 (i arg) (r r1) (i n)
    OpcSetClVal clr r1 n   -> instructionRRR 19 (r clr) (r r1) (i n)
    OpcEq r0 r1 r2         -> instructionRRR 20 (r r0) (r r1) (r r2)
    OpcCopySym r0 r1       -> instructionRRR 21 (r r0) (r r1) (i 0)
    OpcSetSymField r0 r1 n -> instructionRRR 22 (r r0) (r r1) (i n)
    OpcLoadStr r0 a        -> instructionRI  23 (r r0) (caddr a)
    OpcStrLen r0 r1        -> instructionRRR 24 (r r0) (r r1) 0
    OpcNewStr r0 r1        -> instructionRRR 25 (r r0) (r r1) 0
    OpcGetChar r0 r1 r2    -> instructionRRR 26 (r r0) (r r1) (r r2)
    OpcPutChar r0 r1 r2    -> instructionRRR 27 (r r0) (r r1) (r r2)
    OpcLT r0 r1 r2         -> instructionRRR 28 (r r0) (r r1) (r r2)
    OpcGT r0 r1 r2         -> instructionRRR 29 (r r0) (r r1) (r r2)
    OpcJmpTrue r0 n        -> instructionRI  30 (r r0) (biasNumber n)
    OpcOr r0 r1 r2         -> instructionRRR 31 (r r0) (r r1) (r r2)
    OpcAnd r0 r1 r2        -> instructionRRR 32 (r r0) (r r1) (r r2)
    OpcNot r0 r1           -> instructionRRR 33 (r r0) (r r1) (r 0)
    OpcFunHeader arity     -> instructionRI  63 (r 0) (i arity)


instBits, opcBits, regBits :: Int
instBits = 32
opcBits = 6
regBits = 5

biasNumber :: Int -> Int
biasNumber n = n + numberBias

-- an instruction containing a register and a number
instructionRI :: Int -> Int -> Int -> VMWord
instructionRI opcId register value =
  fromIntegral $
  (opcId `shiftL` (instBits - opcBits))
  .|. (register `shiftL` (instBits - (opcBits + regBits)))
  .|. value


-- an instruction containing three registers
instructionRRR :: Int -> Int -> Int -> Int -> VMWord
instructionRRR opcId r0 r1 r2 =
  fromIntegral $
  (opcId `shiftL` (instBits - opcBits))
  .|. (r0 `shiftL` (instBits - (opcBits + regBits)))
  .|. (r1 `shiftL` (instBits - (opcBits + 2 * regBits)))
  .|. (r2 `shiftL` (instBits - (opcBits + 3 * regBits)))




