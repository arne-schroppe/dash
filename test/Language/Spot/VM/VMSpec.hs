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


    it "adds two numbers" $ do
      let prog = [[ Op_load_i 1 5,
                    Op_load_i 2 32,
                    Op_add 0 1 2,
                    Op_halt ]]
      (runProg prog) `shouldReturn` 37

    it "moves a register" $ do
      let prog = [[ Op_load_i  2 37,
                    Op_move  0 2,
                    Op_halt ]]
      (runProg prog) `shouldReturn` 37

    it "directly calls a function" $ do
      let prog = [[ Op_load_i  1 15,
                    Op_load_i  2 23,
                    Op_add  4 1 2,
                    Op_load_f  3 1,
                    Op_call 0 3 1,
                    Op_halt ], [
                    Op_load_i  2 100,
                    Op_add  0 1 2,
                    Op_ret]]

      (runProg prog) `shouldReturn` 138

    it "calls a closure downwards" $ do
      let prog = [[ Op_load_f 2 2,
                    Op_load_i 3 80,
                    Op_make_cl 2 2 1,
                    Op_load_f 1 1,
                    Op_call 0 1 1,
                    Op_halt ], [
                    -- fun1
                    Op_load_i 2 115,
                    Op_load_i 3 23,
                    Op_add 2 2 3,
                    Op_call_cl 0 1 1,
                    Op_ret ], [
                    -- fun2
                    -- fun_header 1 1, -- (* 1 closed over value, 1 parameter *)
                    Op_sub 0 1 2,
                    Op_ret ]]
      (runProg prog) `shouldReturn` 58 -- 115 + 23 - 80

    it "calls a closure upwards" $ do
      let prog = [[ Op_load_f 1 1,
                    Op_call 1 1 1,
                    Op_load_i 2 80,
                    Op_call_cl 0 1 1,
                    Op_halt ], [
                    -- fun 1
                    Op_load_f 1 2,
                    Op_load_i 2 24,
                    Op_make_cl 0 1 1,
                    Op_ret ], [
                    -- fun 2
                    Op_sub 0 1 2,
                    Op_ret ]]
      (runProg prog) `shouldReturn` 56 -- 80 - 24

{-
    it "applies a number tag to a value" $ do
      let original = 44
      let symbol = make_vm_value original vm_tag_number
      assert_equal (tag_of_vm_value symbol) vm_tag_number,
      assert_equal (value_of_vm_value symbol) original
    ),

    it "applies a symbol tag to a value" $ do
      let original = 12
      let symbol = make_vm_value original vm_tag_symbol
      assert_equal (tag_of_vm_value symbol) vm_tag_symbol,
      assert_equal (value_of_vm_value symbol) original
    ),
-}

{-
    it "loads a symbol into a register" $ do
      let prog = [[ Op_load_s 0 12,
                    Op_halt]]
      (runProg prog) `shouldReturn` (vm_symbol_of_int 12),
    ),

    it "loads a constant" $ do
      let ctable = make_const_table [[
        vm_symbol_of_int 33
      ]]
      let prog = [[
        Op_load_c  0 0,
        Op_halt
      ]]
      (runProg prog) `shouldReturn` (vm_symbol_of_int 33)
    ),

    it "loads a data symbol" $ do
      let prog = [[
        Op_load_sd  0 1,
        Op_halt
      ]]
      (runProg prog) `shouldReturn` (vm_data_symbol_of_int 1)
    ),

    it "jumps forward" $ do
      let prog = [[
        Op_load_i 0 66,
        Op_jmp(1),
        Op_halt,
        Op_load_i 0 70,
        Op_halt
      ]]
      (runProg prog) `shouldReturn` 70
    ),

    it "matches a number" $ do
      let ctable = make_const_table [[
        vm_match_header 2,
        vm_number_of_int 11,
        vm_number_of_int 22
      ]]
      let prog = [[
        Op_load_i 0 600,
        Op_load_i 1 22,
        Op_load_i 2 0,
        Op_match 1 2 0,
        Op_jmp(1),
        Op_jmp(2),
        Op_load_i 0 4,
        Op_halt,
        Op_load_i 0 300,
        Op_halt
      ]]
      (runProg prog) `shouldReturn` 300
    ),

    it "matches a symbol" $ do
      let ctable = make_const_table [[
        vm_match_header 2,
        vm_symbol_of_int 11,
        vm_symbol_of_int 22
      ]]
      let prog = [[
        Op_load_i 0 600,
        Op_load_s 1 22,
        Op_load_i 2 0,
        Op_match 1 2 0,
        Op_jmp(1),
        Op_jmp(2),
        Op_load_i 0 4,
        Op_halt,
        Op_load_i 0 300,
        Op_halt
      ]]
      (runProg prog) `shouldReturn` 300
    ),

    it "matches a data symbol" $ do
      let ctable = make_const_table [[
        vm_match_header 2,
        vm_data_symbol_of_int 3,
        vm_data_symbol_of_int 6,
        vm_data_symbol_header 1 2,
        vm_number_of_int 55,
        vm_number_of_int 66,
        vm_data_symbol_header 1 2,
        vm_number_of_int 55,
        vm_number_of_int 77,
        vm_data_symbol_header 1 2,
        vm_number_of_int 55,
        vm_number_of_int 77,
      ]]
      let prog = [[
        Op_load_i 0 600,
        Op_load_sd 1 9,
        Op_load_i 2 0,
        Op_match 1 2 0,
        Op_jmp(1),
        Op_jmp(2),
        Op_load_i 0 4,
        Op_halt,
        Op_load_i 0 300,
        Op_halt
      ]]
      (runProg prog) `shouldReturn` 300
    ),

    it "binds a value in a match" $ do
      let ctable = make_const_table [[
        vm_match_header 2,
        vm_data_symbol_of_int 3,
        vm_data_symbol_of_int 6,
        vm_data_symbol_header 1 2,
        vm_number_of_int 55,
        vm_number_of_int 66,
        vm_data_symbol_header 1 2,
        vm_number_of_int 55,
        vm_match_var 1,
        vm_data_symbol_header 1 2,
        vm_number_of_int 55,
        vm_number_of_int 77,
      ]]
      let prog = [[
        Op_load_i 0 600,
        Op_load_i 4 66,
        Op_load_sd 1 9,
        Op_load_i 2 0,
        Op_match 1 2 3,
        Op_jmp(1),
        Op_jmp(2),
        Op_load_i 0 22,
        Op_halt,
        Op_move 0 4,
        Op_halt
      ]]
      (runProg prog) `shouldReturn` 77
    ),

-}

