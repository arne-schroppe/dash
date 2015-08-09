module Language.Dash.CodeGen.BuiltInFunctions (
  builtInFunctions
, bifStringConcatName
) where

import Language.Dash.IR.Data
import Language.Dash.IR.Opcode

bifStringConcatName, bifStringLengthName, bifSubStringName :: String
bifStringConcatName = "$string-concat"
bifStringLengthName = "string-length"
bifSubStringName = "sub-string"

builtInFunctions :: [(Name, Int, [Opcode])]
builtInFunctions = [  (bifStringConcatName, 2, [
                        OpcStrLen 2 0,
                        OpcStrLen 3 1,
                        OpcAdd 4 2 3,
                        OpcNewStr 5 4,
                        OpcLoadI 6 0,  -- index
                        OpcLoadI 7 1,
                        -- loop1:
                        OpcLT 8 2 6, -- is index >= length?
                        OpcJmpTrue 8 4, -- jmp next
                        OpcGetChar 9 0 6,
                        OpcPutChar 9 5 6,
                        OpcAdd 6 6 7,
                        OpcJmp (-6), -- jmp loop1
                        -- next:
                        OpcLoadI 6 0,
                        -- loop2:
                        OpcLT 8 3 6,
                        OpcJmpTrue 8 5, -- jmp done
                        OpcGetChar 9 1 6,
                        OpcAdd 10 2 6,
                        OpcPutChar 9 5 10,
                        OpcAdd 6 6 7,
                        OpcJmp (-7), -- jmp loop2
                        -- done:
                        OpcMove 0 5,
                        OpcRet 0
                      ]),
                      (bifStringLengthName, 1, [
                        OpcStrLen 0 0,
                        OpcRet 0
                      ]),
                      (bifSubStringName, 3, [
                        OpcNewStr 3 1,
                        OpcLoadI 6 1,
                        OpcLoadI 7 0, -- counter
                        -- loop1:
                        OpcAdd 9 7 6,  -- TODO this is ugly. Fix our off-by-one error properly
                        OpcLT 4 1 9, -- is index >= length?
                        OpcJmpTrue 4 5, -- jmp next
                        OpcAdd 8 7 0,
                        OpcGetChar 5 2 8,
                        OpcPutChar 5 3 7,
                        OpcAdd 7 7 6,
                        OpcJmp (-8), -- jmp loop1
                        -- next:
                        OpcMove 0 3,
                        OpcRet 0
                      ])
                   ]


