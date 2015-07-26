module Language.Dash.VM.VM (
  execute
) where

import           Data.Word
import           Foreign.C
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Language.Dash.IR.Data  (SymbolNameList)
import           Language.Dash.VM.Types

execute :: [VMWord] -> [VMWord] -> SymbolNameList -> IO (VMWord, [VMWord], SymbolNameList)
execute prog ctable symNames =
  withArray (map CUInt prog) (\progPtr ->
    withArray (map CUInt ctable) (\ctablePtr ->
      vmExecuteForeign progPtr
                       (fromIntegral $ length prog)
                       ctablePtr (fromIntegral $ length ctable) ))
  >>= \a ->
    return (a, ctable, symNames)

-- This will not call back into Haskell, so we can mark it unsafe
foreign import ccall unsafe "vm_execute" vmExecuteForeign
    :: Ptr CUInt -> CInt -> Ptr CUInt -> CInt -> IO Word32

