module Language.Dash.BuiltIn.BuiltInDefinitions (
  builtInFunctions
, builtInSymbols
, bifStringConcatName
, tupleSymbolName
, listConsSymbolName
, listEmptySymbolName
, trueSymbolName
, falseSymbolName
, preamble
, moduleOwner
) where

import           Data.Maybe              (fromJust)
import           Language.Dash.IR.Data
import           Language.Dash.IR.Opcode


tupleSymbolName, listConsSymbolName, listEmptySymbolName, trueSymbolName, falseSymbolName, numberTypeSymbolName, stringTypeSymbolName, symbolTypeSymbolName, functionTypeSymbolName :: String
trueSymbolName = "true"
falseSymbolName = "false"
tupleSymbolName = "tuple"
listConsSymbolName = "list"
listEmptySymbolName = "empty-list"

numberTypeSymbolName = "number"
stringTypeSymbolName = "string"
symbolTypeSymbolName = "symbol"
functionTypeSymbolName = "function"

builtInSymbols :: [(String, SymId)]
builtInSymbols = map f d
  where
    f (s, i) = (s, mkSymId i)
    d = zip syms [0..length syms]
    syms = [ falseSymbolName
           , trueSymbolName
           , "_internal_io" -- TODO prevent user from accessing these directly
           , "eof"
           , "error"

           , numberTypeSymbolName
           , stringTypeSymbolName
           , symbolTypeSymbolName
           , functionTypeSymbolName
           ]

moduleOwner :: SymId
moduleOwner = mkSymId 0


bifStringConcatName, bifStringLengthName, bifSubStringName :: String
bifStringConcatName = "$string-concat"
bifStringLengthName = "string-length"
bifSubStringName = "sub-string"


-- TODO instead of "special" handling for primops, we could
-- also add a bif for every primop and then inline some functions
-- (e.g. those with less than n opcodes, etc)
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
                        OpcJmpTrue 8 4, -- jmp to next
                        OpcGetChar 9 0 6,
                        OpcPutChar 9 5 6,
                        OpcAdd 6 6 7,
                        OpcJmp (-6), -- jmp to loop1
                        -- next:
                        OpcLoadI 6 0,
                        -- loop2:
                        OpcLT 8 3 6,
                        OpcJmpTrue 8 5, -- jmp to done
                        OpcGetChar 9 1 6,
                        OpcAdd 10 2 6,
                        OpcPutChar 9 5 10,
                        OpcAdd 6 6 7,
                        OpcJmp (-7), -- jmp to loop2
                        -- done:
                        OpcMove 0 5,
                        OpcRet 0
                      ]),
                      (bifStringLengthName, 1, [
                        OpcStrLen 0 0,
                        OpcRet 0
                      ]),
                      (bifSubStringName, 3, [ -- TODO truncate length if needed
                        OpcNewStr 3 1,
                        OpcLoadI 6 1,
                        OpcLoadI 7 0, -- counter
                        -- loop1:
                        OpcAdd 9 7 6,  -- TODO this is ugly. Fix our off-by-one error properly
                        OpcLT 4 1 9, -- is index >= length?
                        OpcJmpTrue 4 5, -- jmp to next
                        OpcAdd 8 7 0,
                        OpcGetChar 5 2 8,
                        OpcPutChar 5 3 7,
                        OpcAdd 7 7 6,
                        OpcJmp (-8), -- jmp to loop1
                        -- next:
                        OpcMove 0 3,
                        OpcRet 0
                      ]),
                      ("<=", 2, [
                        OpcLT 2 0 1,
                        OpcJmpTrue 2 1,
                        OpcEq 2 0 1,
                        OpcMove 0 2,
                        OpcRet 0
                      ]),
                      (">=", 2, [
                        OpcGT 2 0 1,
                        OpcJmpTrue 2 1,
                        OpcEq 2 0 1,
                        OpcMove 0 2,
                        OpcRet 0
                      ]),
                      ("to-number", 1, [
                        OpcLoadPS 1 (fromJust $ lookup numberTypeSymbolName builtInSymbols),
                        OpcConvert 0 0 1,
                        OpcRet 0
                      ]),
                      ("to-string", 1, [
                        OpcLoadPS 1 (fromJust $ lookup stringTypeSymbolName builtInSymbols),
                        OpcConvert 0 0 1,
                        OpcRet 0
                      ])
                   ]


returnActionId, readLineActionId, printLineActionId :: Int
returnActionId = 0
readLineActionId = 1
printLineActionId = 2

preamble :: String
preamble = "\n\
\  io = module                                             \n\
\    bind action next =                                    \n\
\      match action with                                   \n\
\        :_internal_io type param :nil -> :_internal_io type param next  \n\
\        :_internal_io type param n0 -> :_internal_io type param (.\\ x = bind (n0 x) next)  \n\
\        _ -> :error \"io-bind: Expected an io action as first argument\" \n\
\      end                                                 \n\
\                                                          \n\
\    return a =                                            \n\
\      :_internal_io " ++ show returnActionId ++ " a :nil       \n\
\                                                          \n\
\    read-ln =                                             \n\
\      :_internal_io " ++ show readLineActionId ++ " :nil :nil     \n\
\                                                          \n\
\    print a =                                             \n\
\      :_internal_io " ++ show printLineActionId ++ " a :nil    \n\
\                                                          \n\
\    print-ln a =                                       \n\
\      :_internal_io " ++ show printLineActionId ++ " (a ++ \"\\n\") :nil    \n\
\  end                                                   \n\
\                                                        \n\
\  head ls =                                             \n\
\    match ls with                                       \n\
\      [a | _] -> a                                      \n\
\      _ -> :error \"Empty list!\"                       \n\
\    end                                                 \n\
\                                                        \n\
\  tail ls =                                             \n\
\    match ls with                                       \n\
\      [_ | as] -> as                                    \n\
\      _ -> []                                           \n\
\    end                                                 \n\
\                                                        \n\
\  map f ls =                                            \n\
\    match ls with                                       \n\
\      [] -> []                                          \n\
\      [a|rest] -> [f a|map f rest]                      \n\
\    end                                                 \n\
\                                                        \n\
\  foldr f z ls =                                        \n\
\    match ls with                                       \n\
\      [] -> z                                           \n\
\      [a|rest] -> f a (foldr f z rest)                  \n\
\    end                                                 \n\
\                                                        \n\
\  concatenate a b =                                     \n\
\    match a with                                        \n\
\      []      -> b                                      \n\
\      [hd|tl] -> [hd | concatenate tl b]                \n\
\    end                                                 \n\
\                                                        \n\
\  reverse l =                                           \n\
\    rev-list' l acc =                                   \n\
\      match l with                                      \n\
\        []      -> acc                                  \n\
\        [hd|tl] -> rev-list' tl [hd | acc]              \n\
\      end                                               \n\
\    rev-list' l []                                      \n\
\                                                        \n\
\                                                        \n\
\  filter f ls =                                         \n\
\    match ls with                                       \n\
\      []     -> []                                      \n\
\      [x|xs] ->                                         \n\
\        if f x                                          \n\
\          then [x|filter f xs]                          \n\
\          else filter f xs                              \n\
\    end                                                 \n\
\                                                        \n\
\  length list =                                         \n\
\    len' l acc =                                        \n\
\      match l with                                      \n\
\        []     -> acc                                   \n\
\        [_|as] -> len' as (acc + 1)                     \n\
\        x      -> :error \"Not a list\"                 \n\
\      end                                               \n\
\    len' list 0                                         \n\
\                                                        \n\
\                                                        \n\
\  sequence m ms =                                       \n\
\    k a b =                                             \n\
\      do m with                                         \n\
\        l  <- a                                         \n\
\        ls <- b                                         \n\
\        return [l|ls]                                   \n\
\      end                                               \n\
\    foldr k (m.return []) ms                            \n\
\                                                        \n\
\  m-map m action ls =                                   \n\
\    sequence m (map action ls)                          \n\
\                                                        "
