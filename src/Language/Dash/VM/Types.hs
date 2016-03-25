module Language.Dash.VM.Types where

import Data.Word
import Data.List (intercalate)
import Data.List.Split (chunksOf)

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
      VMSymbol "$_empty_list" [] -> "[]"
      VMSymbol s [] -> ":" ++ s
      VMSymbol "$_list" fields -> showNestedList fields
      VMSymbol "$_tuple" fields -> "(" ++ intercalate ", " (map show fields) ++ ")"
      VMSymbol "$_record" fields -> "{" ++ intercalate ", " (recordFields fields) ++ "}"
      VMSymbol s fields ->  ":" ++ s ++ "<" ++ intercalate ", " (map showField fields) ++ ">"


showField :: VMValue -> String
showField v =
  case v of
    s@(VMSymbol _ (_:_)) -> "(" ++ show s ++ ")"
    _ -> show v

-- TODO Add better output for invalid lists
showNestedList :: [VMValue] -> String
showNestedList vs =
  "[" ++ intercalate ", " (flatValues (VMSymbol "$_list" vs)) ++ "]"
  where
    flatValues (VMSymbol "$_list" [a, VMSymbol "$_empty_list" []]) = [show a]
    flatValues (VMSymbol "$_list" [a, as@(VMSymbol "$_list" _)]) = show a : flatValues as
    flatValues (VMSymbol "$_list" [a, xs]) = show a : (["!<"] ++ flatValues xs ++ [">!"])
    flatValues x = [show x]

recordFields :: [VMValue] -> [String]
recordFields symbolBody =
  map ( \(VMSymbol k [], v) -> k ++ " = " ++ (show v)) kvPairs
  where
    kvPairs = map ( \[a, b] -> (a, b) ) $ chunksOf 2 symbolBody


