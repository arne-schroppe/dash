module Language.Dash.VM.VM (
  execute
, getVMHeapArray
, getVMHeapValue
) where

import           Data.Word
import           Foreign.C
import           Foreign.Storable
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Language.Dash.IR.Data  (SymbolNameList)
import           Language.Dash.VM.Types

execute :: [VMWord] -> [VMWord] -> SymbolNameList -> IO (VMWord, [VMWord], SymbolNameList)
execute prog ctable symNames =
  withArray (map CUInt prog) (\progPtr ->
    withArray (map CUInt ctable) (\ctablePtr ->
      foreignVMExecute progPtr
                       (fromIntegral $ length prog)
                       ctablePtr (fromIntegral $ length ctable)
  ))
  >>= \a ->
    return (a, ctable, symNames)

getVMHeapValue :: VMWord -> IO VMWord
getVMHeapValue addr = do
  let ptr = foreignVMGetHeapPointer addr
  peek ptr

getVMHeapArray :: VMWord -> Int -> IO [VMWord]
getVMHeapArray addr len = do
  let ptr = foreignVMGetHeapPointer addr
  peekArray len ptr

-- This will not call back into Haskell, so we can mark it unsafe
foreign import ccall unsafe "vm_execute" foreignVMExecute
    :: Ptr CUInt -> CInt -> Ptr CUInt -> CInt -> IO Word32

foreign import ccall unsafe "vm_get_heap_pointer" foreignVMGetHeapPointer
    :: Word32 -> Ptr VMWord

