module Language.Dash.VM.Types where

import Data.Word
import Data.List (intercalate)

type VMWord = Word32

data VMValue =
    VMNumber Int
  | VMSymbol String [VMValue]
  | VMString String
  | VMClosure -- TODO add meaningful data 
  | VMFunction -- TODO add meaningful data (name, arguments, etc)
  | VMOpaqueSymbol
  deriving (Eq)



instance Show VMValue where
  show v =
    case v of
      VMNumber i -> show i
      VMString s -> "\"" ++ s ++ "\""
      VMClosure -> "<closure>"
      VMFunction -> "<function>"
      VMOpaqueSymbol -> "<opaque symbol>"  -- TODO special handling for modules
      VMSymbol "empty-list" [] -> "[]"
      VMSymbol s [] -> ":" ++ s
      VMSymbol "list" fields -> showNestedList fields
      VMSymbol "tuple" fields -> "(" ++ intercalate ", " (map show fields) ++ ")"
      VMSymbol s fields ->  ":" ++ s ++ " " ++ unwords (map showField fields)


showField :: VMValue -> String
showField v =
  case v of
    s@(VMSymbol _ (_:_)) -> "(" ++ show s ++ ")"
    _ -> show v

showNestedList :: [VMValue] -> String
showNestedList vs =
  "[" ++ intercalate ", " (flatValues (VMSymbol "list" vs)) ++ "]"
  where
    flatValues (VMSymbol "list" [a, VMSymbol "empty-list" []]) = [show a]
    flatValues (VMSymbol "list" [a, as@(VMSymbol "list" _)]) = show a : flatValues as
    flatValues (VMSymbol "list" [a, xs]) = show a : (["!<"] ++ flatValues xs ++ [">!"])
    flatValues x = [show x]


