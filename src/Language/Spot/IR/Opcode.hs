module Language.Spot.IR.Opcode where

import Language.Spot.VM.Types

type ConstTable = [VMWord]
type SymbolNameList = [String]

-- TODO make VMWord and VMReg more typesafe and check range when constructing it

type VMReg = Int

{-
instance Num VMReg where
  (+) a b = MkReg $ (+) (getReg a) (getReg b)
  (-) a b = MkReg $ (-) (getReg a) (getReg b)
  (*) a b = MkReg $ (*) (getReg a) (getReg b)
  negate = MkReg . negate . getReg
  abs = MkReg . abs . getReg
  signum = MkReg . signum . getReg
  fromInteger = MkReg . fromIntegral

instance Real VMReg where
  toRational = toRational . getReg

instance Integral VMReg where
  quot a b = MkReg $ quot (getReg a) (getReg b)
  rem a b = MkReg $ rem (getReg a) (getReg b)
  div a b = MkReg $ div (getReg a) (getReg b)
  mod a b = MkReg $ mod (getReg a) (getReg b)
  quotRem a b = let (a', b') = quotRem (getReg a) (getReg b) in
               (MkReg a', MkReg b')
  divMod a b = let (a', b') = divMod (getReg a) (getReg b) in
               (MkReg a', MkReg b')
  toInteger = fromIntegral . getReg
-}


data Opcode =
    Op_ret
  | Op_load_i VMReg VMWord
  | Op_load_f VMReg Int
  | Op_load_s VMReg VMWord
  | Op_load_sd VMReg VMWord
  | Op_load_c VMReg VMWord
  | Op_add VMReg VMReg VMReg
  | Op_sub VMReg VMReg VMReg
  | Op_move VMReg VMReg
  | Op_call VMReg VMReg Int
  | Op_call_cl VMReg VMReg Int
  | Op_make_cl VMReg VMReg Int
  | Op_jmp Int
  | Op_match VMReg VMReg VMReg
  deriving (Show)

