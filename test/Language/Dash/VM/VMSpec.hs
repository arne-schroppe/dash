module Language.Dash.VM.VMSpec where

import           Data.Word
import           Language.Dash.Asm.Assembler
import           Language.Dash.IR.Opcode
import           Language.Dash.IR.Data
import           Language.Dash.VM.DataEncoding
import           Language.Dash.VM.VM
import           Language.Dash.Constants
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
      result <- runProg prog
      decodedResult <- decode result [] []
      decodedResult `shouldBe` (VMNumber 55)


    it "adds two numbers" $ do
      let prog = [[ OpcLoadI 1 5,
                    OpcLoadI 2 32,
                    OpcAdd 0 1 2,
                    OpcRet 0 ]]
      result <- runProg prog
      decodedResult <- decode result [] []
      decodedResult `shouldBe` (VMNumber 37)

    it "moves a register" $ do
      let prog = [[ OpcLoadI  2 37,
                    OpcMove  0 2,
                    OpcRet 0 ]]
      result <- runProg prog
      decodedResult <- decode result [] []
      decodedResult `shouldBe` (VMNumber 37)

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
      result <- runProg prog
      decodedResult <- decode result [] []
      decodedResult `shouldBe` (VMNumber 138)

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
      result <- runProg prog
      decodedResult <- decode result [] []
      decodedResult `shouldBe` (VMNumber 58) -- 115 + 23 - 80

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
      result <- runProg prog
      decodedResult <- decode result [] []
      decodedResult `shouldBe` (VMNumber 56) -- 80 - 24

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
      result <- runProg prog
      decodedResult <- decode result [] []
      decodedResult `shouldBe` (VMNumber 44) -- 77 - 33



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
      result <- runProgTbl ctable prog
      decodedResult <- decode result ctable []
      decodedResult `shouldBe` (VMNumber 33)

    it "loads a compound symbol" $ do
      let ctable = [ encodeNumber 1,
                     encodeCompoundSymbolHeader (mkSymId 5) 1,
                     encodeNumber 3
                   ]
      let prog = [[ OpcLoadCS 0 (mkConstAddr 1),
                    OpcRet 0 ]]
      (runProgTbl ctable prog) `shouldReturn` (encodeCompoundSymbolRef $ mkConstAddr 1)


    it "jumps forward" $ do
      let prog = [[ OpcLoadI 0 66,
                    OpcJmp 1,
                    OpcRet 0,
                    OpcLoadI 0 70,
                    OpcRet 0 ]]
      result <- runProg prog
      decodedResult <- decode result [] []
      decodedResult `shouldBe` (VMNumber 70)


    it "jumps if condition true" $ do
      let prog = [[ OpcLoadI 1 2, -- counter
                    OpcLoadI 2 5, -- target value
                    OpcLoadI 5 0, -- accumulator
                    OpcLoadI 3 1,
                    OpcEq 4 1 2,
                    OpcJmpTrue 4 3,
                    OpcAdd 5 5 1,
                    OpcAdd 1 1 3,
                    OpcJmp (-5),
                    OpcMove 0 5,
                    OpcRet 0 ]]
      result <- runProg prog
      decodedResult <- decode result [] []
      -- result: 2 + 3 + 4 = 9
      decodedResult `shouldBe` (VMNumber 9)


    it "matches a number" $ do
      let ctable = [ encodeMatchHeader 2,
                     encodeNumber 11,
                     encodeNumber 22 ]
      let prog = [[ OpcLoadI 0 600,
                    OpcLoadI 1 22,
                    OpcLoadAddr 2 (mkConstAddr 0),
                    OpcMatch 1 2 0,
                    OpcJmp 1,
                    OpcJmp 2,
                    OpcLoadI 0 4,
                    OpcRet 0,
                    OpcLoadI 0 300,
                    OpcRet 0 ]]
      result <- runProgTbl ctable prog
      decodedResult <- decode result ctable []
      decodedResult `shouldBe` (VMNumber 300)

    it "matches a symbol" $ do
      let ctable = [ encodeMatchHeader 2,
                     encodePlainSymbol (mkSymId 11),
                     encodePlainSymbol (mkSymId 22) ]
      let prog = [[ OpcLoadI 0 600,
                    OpcLoadPS 1 (mkSymId 22),
                    OpcLoadAddr 2 (mkConstAddr 0),
                    OpcMatch 1 2 0,
                    OpcJmp 1,
                    OpcJmp 2,
                    OpcLoadI 0 4,
                    OpcRet 0,
                    OpcLoadI 0 300,
                    OpcRet 0 ]]
      result <- runProgTbl ctable prog
      decodedResult <- decode result ctable []
      decodedResult `shouldBe` (VMNumber 300)

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
                    OpcLoadAddr 2 (mkConstAddr 0),
                    OpcMatch 1 2 0,
                    OpcJmp 1,
                    OpcJmp 2,
                    OpcLoadI 0 4,
                    OpcRet 0,
                    OpcLoadI 0 300,
                    OpcRet 0 ]]
      result <- runProgTbl ctable prog
      decodedResult <- decode result ctable []
      decodedResult `shouldBe` (VMNumber 300)

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
                    OpcLoadAddr 2 (mkConstAddr 0),
                    OpcMatch 1 2 3,
                    OpcJmp 1,
                    OpcJmp 2,
                    OpcLoadI 0 22,
                    OpcRet 0,
                    OpcMove 0 4, -- reg 4 contains match var 1 (see pattern in ctable)
                    OpcRet 0 ]]
      result <- runProgTbl ctable prog
      decodedResult <- decode result [] []
      decodedResult `shouldBe` (VMNumber 77)

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
      result <- runProgTbl ctable prog
      let symNames = ["", "A", "", "B"]
      let decodeResult = decode result ctable symNames
      decodeResult `shouldReturn` (VMSymbol "B" [VMNumber 33, VMNumber 44])

    it "modifies a heap symbol" $ do
      let ctable = [ encodeCompoundSymbolHeader (mkSymId 1) 2,
                     encodeNumber 55,
                     encodeNumber 66,
                     encodeCompoundSymbolHeader (mkSymId 3) 2,
                     encodeNumber 33,
                     encodeNumber 44 ]
      let prog = [[ OpcLoadCS 0 (mkConstAddr 0),
                    OpcLoadCS 1 (mkConstAddr 3),
                    OpcCopySym 0 1,
                    OpcLoadPS 5 (mkSymId 6),
                    OpcSetSymField 0 5 1,
                    OpcRet 0 ]]
      result <- runProgTbl ctable prog
      let symNames = ["", "A", "", "B", "", "", "success"]
      let decodeResult = decode result ctable symNames
      decodeResult `shouldReturn` (VMSymbol "B" [VMNumber 33, VMSymbol "success" []])


    it "loads a string into a register" $ do
      let prog = [[ OpcLoadStr 0 (mkConstAddr 55),
                    OpcRet 0 ]]
      (runProg prog) `shouldReturn` (encodeStringRef $ mkConstAddr 55)

    it "determines the length of a string" $ do
      let ctable = [ encodeStringHeader 5 2,
                     encodeStringChunk 'd' 'a' 's' 'h',
                     encodeStringChunk '!' '\0' '\0' '\0' ]
      let prog = [[ OpcLoadStr 1 (mkConstAddr 0),
                    OpcStrLen 0 1,
                    OpcRet 0 ]]
      result <- runProgTbl ctable prog
      decodedResult <- decode result ctable []
      decodedResult `shouldBe` (VMNumber 5)

    it "creates a new string" $ do
      let prog = [[ OpcLoadI 1 8,
                    OpcNewStr 0 1,
                    OpcRet 0 ]]
      result <- runProg  prog
      decodedResult <- decode result [] []
      decodedResult `shouldBe` (VMString "")

    it "copies a string" $ do
      let loop = (-6);
      let end = 4;
      let ctable = [ encodeStringHeader 5 2,
                     encodeStringChunk 'd' 'a' 's' 'h',
                     encodeStringChunk '!' '\0' '\0' '\0' ]
      let prog = [[ OpcLoadStr 6 (mkConstAddr 0),
                    OpcStrLen 1 6,
                    OpcLoadI 2 0, -- index
                    OpcLoadI 5 1,
                    OpcNewStr 3 1,
                    -- loop:
                    OpcEq 7 2 1,
                    OpcJmpTrue 7 end,
                    OpcGetChar 4 6 2,
                    OpcPutChar 4 3 2,
                    OpcAdd 2 2 5,
                    OpcJmp loop,
                    -- end:
                    OpcMove 0 3,
                    OpcRet 0 ]]
      result <- runProgTbl ctable prog
      decodedResult <- decode result ctable []
      decodedResult `shouldBe` (VMString "dash!")


