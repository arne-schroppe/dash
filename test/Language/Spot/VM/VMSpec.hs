module Language.Spot.VM.VMSpec where

import Test.Hspec

import Language.Spot.IR.Opcode
import Language.Spot.VM.OpcodeAsm
import Language.Spot.VM.VM


spec :: Spec
spec = do
  describe "VM" $ do

    it "does nothing" $ do
      pending
