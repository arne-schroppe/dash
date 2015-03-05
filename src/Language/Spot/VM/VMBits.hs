module Language.Spot.VM.VMBits (
  VMValue(..)
, encode
, decode
, vmMatchHeader
, vmMatchVar
, vmDataSymbolHeader

) where

import Data.Word
import Data.Bits

data VMValue =
    VMNumber Word32 -- Number
  | VMSymbol Word32 -- symbolId
  | VMDataSymbol Word32 -- address
  deriving (Show, Eq)

encode :: VMValue -> Word32
encode v =
  case v of
  VMNumber n -> makeVMValue tagNumber $ ensureRange n
  VMSymbol i -> makeVMValue tagSymbol $ ensureRange i
  VMDataSymbol a -> makeVMValue tagDataSymbol $ ensureRange a
  where ensureRange v = if v < 0 || v > 0x0FFFFFFF then error "Value outside of range" else v

decode :: Word32 -> VMValue
decode w =
  let tag = getTag w in
  let value = getValue w in
  decode' tag value
  where decode' t v | t==tagNumber = VMNumber v
                    | t==tagSymbol = VMSymbol v
                    | t==tagDataSymbol = VMDataSymbol v
                    | otherwise    = error $ "Unknown tag " ++ (show t)

vmMatchHeader :: Word32 -> Word32
vmMatchHeader n = matchData 1 n

vmMatchVar :: Word32 -> Word32
vmMatchVar n = matchData 0 n

matchData mtag n =
  let cropped = n .&. 0x7FFFFFF in
  let mtagVal = mtag `shiftL` (32 - 5) in
  makeVMValue tagMatchData (cropped .|. mtagVal)

vmDataSymbolHeader :: Word32 -> Word32 -> Word32
vmDataSymbolHeader symId n = (symId `shiftL` 16) .|. n


makeVMValue :: Word32 -> Word32 -> Word32
makeVMValue tag i = i .|. (tag `shiftL` (32 - 4))


getTag v = (v .&. 0xF0000000) `rotateL` 4
getValue v = v .&. 0x0FFFFFFF


tagNumber = 0x0 :: Word32
tagSymbol = 0x4 :: Word32
tagDataSymbol = 0x5 :: Word32
tagMatchData = 0xF :: Word32


