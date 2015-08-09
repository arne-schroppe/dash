module Language.Dash.CodeGen.BuiltInFunctions (
  builtInFunctions
, bifStringConcatName
) where

import Language.Dash.IR.Data
import Language.Dash.IR.Opcode

bifStringConcatName :: String
bifStringConcatName = "$string-concat"

builtInFunctions :: [(Name, Int, [Opcode])]
builtInFunctions = [ (bifStringConcatName, 2, [
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
                     ])
                   ]


{-
what we need to implement this:

op_new_string r0 length
op_read_char r0 source index
op_write_char target char_reg index

and some way to loop (jmp backwards and jmp_lt)

-- r0 and r1 are string arguments

r2 <- str_len r0
r3 <- str_len r1
r4 <- add r2 r3
r5 <- new_string r4
r6 <- load_i 0
r7 <- load_i 1
loop1:
jmp_gte r6 r2 next  -- we could turn this into a jmp_eq and then use existing ops
r8 <- read_char r0 r6
write_char r5 r8 r6
r6 <- add r6 r7
jmp loop1
next:
r6 <- load_i 0
loop2:
jmp_gte r6 r3 done
r8 <- read_char r1 r6
r9 <- add r2 r6
write_char r5 r8 r9
r6 <- add r6 r7
jmp loop2
done:
r0 <- move r5
ret r0



-}

