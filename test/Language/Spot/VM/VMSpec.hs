module Language.Spot.VM.VMSpec where

import Test.Hspec

import Language.Spot.IR.Opcode
import Language.Spot.VM.OpcodeAsm
import Language.Spot.VM.VM

import Language.Spot.VM.VMRaw
import Foreign.Ptr
import Foreign.C
import Foreign.Marshal.Array


spec :: Spec
spec = do
  describe "VM" $ do

    it "does nothing" $ do
      withArray [1, 0] (\a -> vmExecute a nullPtr) `shouldReturn` 0
