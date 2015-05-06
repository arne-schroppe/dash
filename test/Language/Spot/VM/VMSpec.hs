module Language.Spot.VM.VMSpec where

import Test.Hspec
import Test.QuickCheck

import Data.Word

import Language.Spot.IR.Tac
import Language.Spot.VM.Assembler
import Language.Spot.VM.VM
import Language.Spot.VM.Bits

runProg :: [[Tac Reg]] -> IO Word32
runProg = runProgTbl []

runProgTbl :: [Word32] -> [[Tac Reg]] -> IO Word32
runProgTbl tbl prog = do
  (value, _, _) <- execute asm tbl' []
  return value
  where (asm, tbl', _) = assembleWithEncodedConstTable prog tbl fromIntegral []


spec :: Spec
spec = do
  describe "Virtual Machine" $ do

    it "loads a number into a register" $ do
      let prog = [[ Tac_load_i 0 55,
                    Tac_ret 0 ]]
      (runProg prog) `shouldReturn` 55


    it "adds two numbers" $ do
      let prog = [[ Tac_load_i 1 5,
                    Tac_load_i 2 32,
                    Tac_add 0 1 2,
                    Tac_ret 0 ]]
      (runProg prog) `shouldReturn` 37

    it "moves a register" $ do
      let prog = [[ Tac_load_i  2 37,
                    Tac_move  0 2,
                    Tac_ret 0 ]]
      (runProg prog) `shouldReturn` 37

    it "directly calls a function" $ do
      let prog = [[ Tac_load_i  1 15,
                    Tac_load_i  2 23,
                    Tac_add  4 1 2,
                    Tac_load_f  3 1,
                    Tac_set_arg 0 4 0,
                    Tac_call 0 3 1,
                    Tac_ret 0 ], [
                    Tac_load_i  1 100,
                    Tac_add  2 0 1,
                    Tac_ret 2]]

      (runProg prog) `shouldReturn` 138

    it "calls a closure downwards" $ do
      let prog = [[ Tac_load_f 2 2,
                    Tac_load_i 3 80,
                    Tac_make_cl 2 2 1,
                    Tac_load_f 1 1,
                    Tac_set_arg 0 2 0,
                    Tac_call 0 1 1,
                    Tac_ret 0 ], [
                    -- fun1
                    Tac_load_i 2 115,
                    Tac_load_i 3 23,
                    Tac_add 2 2 3,
                    Tac_set_arg 0 2 0,
                    Tac_call_cl 2 0 1,
                    Tac_ret 2 ], [
                    -- fun2
                    -- fun_header 1 1, -- (* 1 closed over value, 1 parameter *)
                    Tac_sub 2 0 1,
                    Tac_ret 2 ]]
      (runProg prog) `shouldReturn` 58 -- 115 + 23 - 80

    it "calls a closure upwards" $ do
      let prog = [[ Tac_load_f 1 1,
                    Tac_call 1 1 0,
                    Tac_load_i 2 80,
                    Tac_set_arg 0 2 0,
                    Tac_call_cl 0 1 1,
                    Tac_ret 0 ], [
                    -- fun 1
                    Tac_load_f 1 2,
                    Tac_load_i 2 24,
                    Tac_make_cl 0 1 1,
                    Tac_ret 0 ], [
                    -- fun 2
                    Tac_sub 2 0 1,
                    Tac_ret 2 ]]
      (runProg prog) `shouldReturn` 56 -- 80 - 24

{-
    it "applies a number tag to a value" $ do
      let original = 44
      let symbol = make_vm_value original vm_tag_number
      (tag_of_vm_value symbol) vm_tag_number,
      assert_equal (value_of_vm_value symbol) original
    ),

    it "applies a symbol tag to a value" $ do
      let original = 12
      let symbol = make_vm_value original vm_tag_symbol
      assert_equal (tag_of_vm_value symbol) vm_tag_symbol,
      assert_equal (value_of_vm_value symbol) original
    ),

-}
    it "loads a symbol into a register" $ do
      let prog = [[ Tac_load_ps 0 12,
                    Tac_ret 0]]
      (runProg prog) `shouldReturn` (encodePlainSymbol 12)

    it "loads a constant" $ do
      let ctable = [ encodeNumber 33 ]
      let prog = [[ Tac_load_c 0 0,
                    Tac_ret 0 ]]
      (runProgTbl ctable prog) `shouldReturn` (33)

    it "loads a data symbol" $ do
      let prog = [[ Tac_load_cs 0 1,
                    Tac_ret 0 ]]
      (runProg prog) `shouldReturn` (encodeCompoundSymbolRef 1)


    it "jumps forward" $ do
      let prog = [[ Tac_load_i 0 66,
                    Tac_jmp 1,
                    Tac_ret 0,
                    Tac_load_i 0 70,
                    Tac_ret 0 ]]
      (runProg prog) `shouldReturn` 70

    it "matches a number" $ do
      let ctable = [ encodeMatchHeader 2,
                     encodeNumber 11,
                     encodeNumber 22 ]
      let prog = [[ Tac_load_i 0 600,
                    Tac_load_i 1 22,
                    Tac_load_i 2 0,
                    Tac_match 1 2 0,
                    Tac_jmp 1,
                    Tac_jmp 2,
                    Tac_load_i 0 4,
                    Tac_ret 0,
                    Tac_load_i 0 300,
                    Tac_ret 0 ]]
      (runProgTbl ctable prog) `shouldReturn` 300

    it "matches a symbol" $ do
      let ctable = [ encodeMatchHeader 2,
                     encodePlainSymbol 11,
                     encodePlainSymbol 22 ]
      let prog = [[ Tac_load_i 0 600,
                    Tac_load_ps 1 22,
                    Tac_load_i 2 0,
                    Tac_match 1 2 0,
                    Tac_jmp 1,
                    Tac_jmp 2,
                    Tac_load_i 0 4,
                    Tac_ret 0,
                    Tac_load_i 0 300,
                    Tac_ret 0 ]]
      (runProgTbl ctable prog) `shouldReturn` 300

    it "matches a data symbol" $ do
      let ctable = [ encodeMatchHeader 2,
                     encodeCompoundSymbolRef 3,
                     encodeCompoundSymbolRef 6,
                     encodeCompoundSymbolHeader 1 2,
                     encodeNumber 55,
                     encodeNumber 66,
                     encodeCompoundSymbolHeader 1 2,
                     encodeNumber 55,
                     encodeNumber 77,
                     encodeCompoundSymbolHeader 1 2,
                     encodeNumber 55,
                     encodeNumber 77 ]
      let prog = [[ Tac_load_i 0 600,
                    Tac_load_cs 1 9,
                    Tac_load_i 2 0,
                    Tac_match 1 2 0,
                    Tac_jmp 1,
                    Tac_jmp 2,
                    Tac_load_i 0 4,
                    Tac_ret 0,
                    Tac_load_i 0 300,
                    Tac_ret 0 ]]
      (runProgTbl ctable prog) `shouldReturn` 300

    it "binds a value in a match" $ do
      let ctable = [ encodeMatchHeader 2,
                     encodeCompoundSymbolRef 3,
                     encodeCompoundSymbolRef 6,
                     encodeCompoundSymbolHeader 1 2,
                     encodeNumber 55,
                     encodeNumber 66,
                     encodeCompoundSymbolHeader 1 2,
                     encodeNumber 55,
                     encodeMatchVar 1,
                     encodeCompoundSymbolHeader 1 2,
                     encodeNumber 55,
                     encodeNumber 77 ]
      let prog = [[ Tac_load_i 0 600,
                    Tac_load_i 4 66,
                    Tac_load_cs 1 9,
                    Tac_load_i 2 0,
                    Tac_match 1 2 3,
                    Tac_jmp 1,
                    Tac_jmp 2,
                    Tac_load_i 0 22,
                    Tac_ret 0,
                    Tac_move 0 4, -- reg 4 contains match var 1 (see pattern in ctable)
                    Tac_ret 0 ]]
      (runProgTbl ctable prog) `shouldReturn` 77

{- TODO
    it "decodes a number" $ property $
      choose (0, 0x0FFFFFFF) >>= \x -> return $ (decode . encodeNumber) x == (VMNumber x)


    it "decodes a symbol" $ property $
      choose (0, 0x0FFFFFFF) >>= \x -> return $ (decode . encodePlainSymbol) x == (VMSymbol x [])
-}


