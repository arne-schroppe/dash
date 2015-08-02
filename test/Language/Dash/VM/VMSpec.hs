module Language.Dash.VM.VMSpec where

import           Data.Word
import           Language.Dash.Asm.Assembler
import           Language.Dash.IR.Opcode
import           Language.Dash.IR.Data
import           Language.Dash.VM.DataEncoding
import           Language.Dash.VM.VM
import           Test.Hspec
import           Test.QuickCheck

runProg :: [[Opcode]] -> IO Word32
runProg = runProgTbl []

runProgTbl :: [Word32] -> [[Opcode]] -> IO Word32
runProgTbl tbl prog = do
  (value, _, _) <- execute asm tbl' []
  return value
  where
    (asm, tbl', _) =
      assembleWithEncodedConstTable prog tbl (fromIntegral.constAddrToInt) []


spec :: Spec
spec = do
  describe "Virtual Machine" $ do

    it "loads a number into a register" $ do
      let prog = [[ OpcLoadI 0 55,
                    OpcRet 0 ]]
      (runProg prog) `shouldReturn` 55


    it "adds two numbers" $ do
      let prog = [[ OpcLoadI 1 5,
                    OpcLoadI 2 32,
                    OpcAdd 0 1 2,
                    OpcRet 0 ]]
      (runProg prog) `shouldReturn` 37

    it "moves a register" $ do
      let prog = [[ OpcLoadI  2 37,
                    OpcMove  0 2,
                    OpcRet 0 ]]
      (runProg prog) `shouldReturn` 37

    it "directly calls a function" $ do
      let prog = [[ OpcLoadI  1 15,
                    OpcLoadI  2 23,
                    OpcAdd  4 1 2,
                    OpcLoadF 3 (mkFuncAddr 1),
                    OpcSetArg 0 4 0,
                    OpcCall 0 3 1,
                    OpcRet 0 ], [
                    OpcFunHeader 1,
                    OpcLoadI  1 100,
                    OpcAdd  2 0 1,
                    OpcRet 2]]

      (runProg prog) `shouldReturn` 138

    it "calls a closure downwards" $ do
      let prog = [[ OpcLoadF 2 (mkFuncAddr 2),
                    OpcLoadI 3 80,
                    OpcSetArg 0 3 0,
                    OpcPartAp 2 2 1,
                    OpcLoadF 1 (mkFuncAddr 1),
                    OpcSetArg 0 2 0,
                    OpcCall 0 1 1,
                    OpcRet 0 ], [
                    -- fun1
                    OpcFunHeader 2,
                    OpcLoadI 2 115,
                    OpcLoadI 3 23,
                    OpcAdd 2 2 3,
                    OpcSetArg 0 2 0,
                    OpcGenAp 2 0 1,
                    OpcRet 2 ], [
                    -- fun2
                    -- fun_header 1 1, -- (* 1 closed over value, 1 parameter *)
                    OpcFunHeader 2,
                    OpcSub 2 1 0,
                    OpcRet 2 ]]
      (runProg prog) `shouldReturn` 58 -- 115 + 23 - 80

    it "calls a closure upwards" $ do
      let prog = [[ OpcLoadF 1 (mkFuncAddr 1),
                    OpcCall 1 1 0,
                    OpcLoadI 2 80,
                    OpcSetArg 0 2 0,
                    OpcGenAp 0 1 1,
                    OpcRet 0 ], [
                    -- fun 1
                    OpcFunHeader 1,
                    OpcLoadF 1 (mkFuncAddr 2),
                    OpcLoadI 2 24,
                    OpcSetArg 0 2 0,
                    OpcPartAp 0 1 1,
                    OpcRet 0 ], [
                    -- fun 2
                    OpcFunHeader 2,
                    OpcSub 2 1 0,
                    OpcRet 2 ]]
      (runProg prog) `shouldReturn` 56 -- 80 - 24

    it "modifies a closure" $ do
      let prog = [[ OpcLoadF 1 (mkFuncAddr 1),
                    OpcCall 1 1 0,
                    OpcLoadI 2 80,
                    OpcSetArg 0 2 0,
                    OpcGenAp 0 1 1,
                    OpcRet 0 ], [
                    -- fun 1
                    OpcFunHeader 1,
                    OpcLoadF 1 (mkFuncAddr 2),
                    OpcLoadI 2 77,
                    OpcLoadI 3 55,
                    OpcSetArg 0 2 1,
                    OpcPartAp 0 1 2,
                    OpcLoadI 7 33,
                    OpcSetClVal 0 7 1,
                    OpcRet 0 ], [
                    -- fun 2
                    OpcFunHeader 3,
                    OpcSub 3 0 1,
                    OpcRet 3 ]]
      (runProg prog) `shouldReturn` 44 -- 77 - 33



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
      let sym = mkSymId 12
      let prog = [[ OpcLoadPS 0 sym,
                    OpcRet 0]]
      (runProg prog) `shouldReturn` (encodePlainSymbol sym)

    it "loads a constant" $ do
      let ctable = [ encodeNumber 33 ]
      let prog = [[ OpcLoadC 0 (mkConstAddr 0),
                    OpcRet 0 ]]
      (runProgTbl ctable prog) `shouldReturn` (33)

    it "loads a data symbol" $ do
      let prog = [[ OpcLoadCS 0 (mkConstAddr 1),
                    OpcRet 0 ]]
      (runProg prog) `shouldReturn` (encodeCompoundSymbolRef $ mkConstAddr 1)


    it "jumps forward" $ do
      let prog = [[ OpcLoadI 0 66,
                    OpcJmp 1,
                    OpcRet 0,
                    OpcLoadI 0 70,
                    OpcRet 0 ]]
      (runProg prog) `shouldReturn` 70

    it "matches a number" $ do
      let ctable = [ encodeMatchHeader 2,
                     encodeNumber 11,
                     encodeNumber 22 ]
      let prog = [[ OpcLoadI 0 600,
                    OpcLoadI 1 22,
                    OpcLoadI 2 0,
                    OpcMatch 1 2 0,
                    OpcJmp 1,
                    OpcJmp 2,
                    OpcLoadI 0 4,
                    OpcRet 0,
                    OpcLoadI 0 300,
                    OpcRet 0 ]]
      (runProgTbl ctable prog) `shouldReturn` 300

    it "matches a symbol" $ do
      let ctable = [ encodeMatchHeader 2,
                     encodePlainSymbol (mkSymId 11),
                     encodePlainSymbol (mkSymId 22) ]
      let prog = [[ OpcLoadI 0 600,
                    OpcLoadPS 1 (mkSymId 22),
                    OpcLoadI 2 0,
                    OpcMatch 1 2 0,
                    OpcJmp 1,
                    OpcJmp 2,
                    OpcLoadI 0 4,
                    OpcRet 0,
                    OpcLoadI 0 300,
                    OpcRet 0 ]]
      (runProgTbl ctable prog) `shouldReturn` 300

    it "matches a data symbol" $ do
      let ctable = [ encodeMatchHeader 2,
                     encodeCompoundSymbolRef (mkConstAddr 3),
                     encodeCompoundSymbolRef (mkConstAddr 6),
                     encodeCompoundSymbolHeader (mkSymId 1) 2,
                     encodeNumber 55,
                     encodeNumber 66,
                     encodeCompoundSymbolHeader (mkSymId 1) 2,
                     encodeNumber 55,
                     encodeNumber 77,
                     encodeCompoundSymbolHeader (mkSymId 1) 2,
                     encodeNumber 55,
                     encodeNumber 77 ]
      let prog = [[ OpcLoadI 0 600,
                    OpcLoadCS 1 (mkConstAddr 9),
                    OpcLoadI 2 0,
                    OpcMatch 1 2 0,
                    OpcJmp 1,
                    OpcJmp 2,
                    OpcLoadI 0 4,
                    OpcRet 0,
                    OpcLoadI 0 300,
                    OpcRet 0 ]]
      (runProgTbl ctable prog) `shouldReturn` 300

    it "binds a value in a match" $ do
      let ctable = [ encodeMatchHeader 2,
                     encodeCompoundSymbolRef (mkConstAddr 3),
                     encodeCompoundSymbolRef (mkConstAddr 6),
                     encodeCompoundSymbolHeader (mkSymId 1) 2,
                     encodeNumber 55,
                     encodeNumber 66,
                     encodeCompoundSymbolHeader (mkSymId 1) 2,
                     encodeNumber 55,
                     encodeMatchVar 1,
                     encodeCompoundSymbolHeader (mkSymId 1) 2,
                     encodeNumber 55,
                     encodeNumber 77 ]
      let prog = [[ OpcLoadI 0 600,
                    OpcLoadI 4 66,
                    OpcLoadCS 1 (mkConstAddr 9),
                    OpcLoadI 2 0,
                    OpcMatch 1 2 3,
                    OpcJmp 1,
                    OpcJmp 2,
                    OpcLoadI 0 22,
                    OpcRet 0,
                    OpcMove 0 4, -- reg 4 contains match var 1 (see pattern in ctable)
                    OpcRet 0 ]]
      (runProgTbl ctable prog) `shouldReturn` 77

    it "loads a symbol on the heap" $ do
      let ctable = [ encodeCompoundSymbolHeader (mkSymId 1) 2,
                     encodeNumber 55,
                     encodeNumber 66,
                     encodeCompoundSymbolHeader (mkSymId 3) 2,
                     encodeNumber 33,
                     encodeNumber 44 ]
      let prog = [[ OpcLoadCS 0 (mkConstAddr 0),
                    OpcLoadCS 1 (mkConstAddr 3),
                    OpcCopySym 0 1,
                    OpcRet 0 ]]
      let heapStart = mkHeapAddr 1
      result <- runProgTbl ctable prog
      let symNames = ["", "A", "", "B"]
      let decodeResult = decode result ctable symNames
      decodeResult `shouldReturn` (VMSymbol "B" [VMNumber 33, VMNumber 44])

{-
    it "modifies a heap symbol" $ do
      let ctable
-}

{-


  vm_value const_table[] = {
    compound_symbol_header(5, 2),
    make_tagged_val(55, vm_tag_number),
    make_tagged_val(66, vm_tag_number),
    compound_symbol_header(7, 2),
    make_tagged_val(33, vm_tag_number),
    make_tagged_val(44, vm_tag_number),
  };
  vm_instruction program[] = {
    op_load_cs(0, 0),
    op_load_cs(1, 3),
    op_copy_sym(0, 1),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), const_table, array_length(const_table));
  is_equal(result, make_tagged_val(heap_start, vm_tag_dynamic_compound_symbol));

  vm_value *heap_p = heap_get_pointer(heap_start);
  vm_value sym_header = *heap_p;
  is_equal(compound_symbol_count(sym_header), 2);
  int header_size = 1;
  is_equal(heap_p[header_size + 0], 33);
  is_equal(heap_p[header_size + 1], 44);
}

it( modifies_a_heap_symbol ) {

  vm_value const_table[] = {
    compound_symbol_header(5, 2),
    make_tagged_val(55, vm_tag_number),
    make_tagged_val(66, vm_tag_number),
    compound_symbol_header(7, 2),
    make_tagged_val(33, vm_tag_number),
    make_tagged_val(44, vm_tag_number),
  };
  vm_instruction program[] = {
    op_load_cs(0, 0),
    op_load_cs(1, 3),
    op_copy_sym(0, 1),
    op_load_ps(5, 77),
    op_set_sym_field(0, 5, 1),
    op_ret(0)
  };
  vm_value result = vm_execute(program, array_length(program), const_table, array_length(const_table));
  is_equal(result, make_tagged_val(heap_start, vm_tag_dynamic_compound_symbol));

  vm_value *heap_p = heap_get_pointer(heap_start);
  vm_value sym_header = *heap_p;
  is_equal(compound_symbol_count(sym_header), 2);
  int header_size = 1;
  is_equal(heap_p[header_size + 0], 33);
  is_equal(heap_p[header_size + 1], make_tagged_val(77, vm_tag_plain_symbol));

-}

{- TODO
    it "decodes a number" $ property $
      choose (0, 0x0FFFFFFF) >>= \x -> return $ (decode . encodeNumber) x == (VMNumber x)


    it "decodes a symbol" $ property $
      choose (0, 0x0FFFFFFF) >>= \x -> return $ (decode . encodePlainSymbol) x == (VMSymbol x [])
-}


