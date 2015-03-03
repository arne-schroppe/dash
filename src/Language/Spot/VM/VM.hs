module Language.Spot.VM.VM where

import Language.Spot.VM.VMRaw
import Data.Word

import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.C

executeVMProgram :: [Word32] -> [Word32] -> IO Word32
executeVMProgram prog ctable = withArray (map CUInt prog) (\p -> vmExecuteForeign p nullPtr) >>= return


