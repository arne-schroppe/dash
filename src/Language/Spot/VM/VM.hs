module Language.Spot.VM.VM (
execute
) where

import Language.Spot.VM.Bits
import Language.Spot.VM.Types
import Language.Spot.IR.Tac
import Data.Word

import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.C



execute :: [VMWord] -> [VMWord] -> SymbolNameList -> IO (VMWord, [VMWord], SymbolNameList)
execute prog ctable symNames = withArray (map CUInt prog) (\progPtr ->
                               withArray (map CUInt ctable) (\ctablePtr ->
                               vmExecuteForeign progPtr ctablePtr)) >>= \a -> return (a, ctable, symNames)

foreign import ccall unsafe "vm_execute" vmExecuteForeign
    :: Ptr CUInt -> Ptr CUInt -> IO Word32

