{-# LANGUAGE ForeignFunctionInterface #-}

module Language.Spot.VM.VMRaw where

import Foreign.C
import Foreign.Ptr

foreign import ccall unsafe "vm_execute" vmExecute
    :: Ptr CUInt -> Ptr CUInt -> IO CUInt

