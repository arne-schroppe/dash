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
                       OpcRet 0
                     ])
                   ]

