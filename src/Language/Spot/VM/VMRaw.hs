{-# LANGUAGE ForeignFunctionInterface #-}

module Language.Spot.VM.VMRaw where

import Foreign.C
import Foreign.Ptr
import Data.Word

foreign import ccall unsafe "vm_execute" vmExecuteForeign
    :: Ptr CUInt -> Ptr CUInt -> IO Word32

