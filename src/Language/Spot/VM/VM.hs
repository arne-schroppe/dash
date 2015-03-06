module Language.Spot.VM.VM (
execute
) where

import Language.Spot.VM.Bits
import Data.Word

import Language.Spot.IR.Opcode
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.C



execute :: [Word32] -> ConstTable -> SymbolNameList -> IO (Word32, SymbolNameList)
execute prog ctable symNames = withArray (map CUInt prog) (\progPtr ->
                               withArray (map CUInt ctable) (\ctablePtr ->
                               vmExecuteForeign progPtr ctablePtr)) >>= \a -> return (a, symNames)

foreign import ccall unsafe "vm_execute" vmExecuteForeign
    :: Ptr CUInt -> Ptr CUInt -> IO Word32

