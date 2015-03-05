module Language.Spot.VM.VM (
execute
) where

import Language.Spot.VM.Bits
import Data.Word

import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.C



execute :: [Word32] -> [Word32] -> IO Word32
execute prog ctable = withArray (map CUInt prog) (\progPtr ->
                               withArray (map CUInt ctable) (\ctablePtr ->
                               vmExecuteForeign progPtr ctablePtr)) >>= return

foreign import ccall unsafe "vm_execute" vmExecuteForeign
    :: Ptr CUInt -> Ptr CUInt -> IO Word32

