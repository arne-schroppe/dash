module Language.Spot.VM.VMSpec where

import Test.Hspec

import Data.Word

import Language.Spot.IR.Opcode
import Language.Spot.VM.OpcodeAsm
import Language.Spot.VM.VM

runProg :: [[Opcode]] -> IO Word32
runProg prog = executeVMProgram asm []
  where asm = assemble prog

spec :: Spec
spec = do
  describe "Virtual Machine" $ do

    it "loads a number into a register" $ do
      let prog = [[ Op_load_i 0 55,
                    Op_halt ]]
      (runProg prog) `shouldReturn` 55
