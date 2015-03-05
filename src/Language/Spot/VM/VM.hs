module Language.Spot.VM.VM where

import Language.Spot.VM.VMRaw
import Language.Spot.VM.VMBits
import Data.Word

import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.C

execute :: [Word32] -> [Word32] -> IO Word32
execute prog ctable = withArray (map CUInt prog) (\progPtr ->
                               withArray (map CUInt ctable) (\ctablePtr ->
                               vmExecuteForeign progPtr ctablePtr)) >>= return


