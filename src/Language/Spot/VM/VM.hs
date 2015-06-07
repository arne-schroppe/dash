module Language.Spot.VM.VM (
  execute
) where

import           Data.Word
import           Foreign.C
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Language.Spot.IR.Data  (SymbolNameList)
import           Language.Spot.VM.Types



execute :: [VMWord] -> [VMWord] -> SymbolNameList -> IO (VMWord, [VMWord], SymbolNameList)
execute prog ctable symNames = withArray (map CUInt prog) (\progPtr ->
                               withArray (map CUInt ctable) (\ctablePtr ->
                               vmExecuteForeign progPtr (fromIntegral $ length prog) ctablePtr (fromIntegral $ length ctable) )) >>= \a -> return (a, ctable, symNames)

foreign import ccall unsafe "vm_execute" vmExecuteForeign
    :: Ptr CUInt -> CInt -> Ptr CUInt -> CInt -> IO Word32

