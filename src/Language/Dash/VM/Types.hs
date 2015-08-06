module Language.Dash.VM.Types where

import Data.Word
import Data.List (intercalate, intersperse)

type VMWord = Word32

data VMValue =
    VMNumber Int
  | VMSymbol String [VMValue]
  | VMString String
  | VMClosure -- TODO add meaningful data 
  | VMFunction -- TODO add meaningful data (name, arguments, etc)
  deriving (Eq)


showField :: VMValue -> String
showField v =
  case v of
    s@(VMSymbol _ (_:_)) -> "(" ++ show s ++ ")"
    _ -> show v

showNestedList :: [VMValue] -> String
showNestedList vs =
  "[" ++ (intercalate ", " $ flatValues (VMSymbol "list" vs)) ++ "]"
  where
    flatValues (VMSymbol "list" [a, (VMSymbol "empty-list" [])]) = [show a]
    flatValues (VMSymbol "list" [a, as]) = (show a) : (flatValues as)
    flatValues x = [show x]

instance Show VMValue where
  show v =
    case v of
      VMNumber i -> show i -- TODO we need to properly convert this to int to show negative numbers
      VMClosure -> "<closure>"
      VMFunction -> "<function>"
      VMSymbol "empty-list" [] -> "[]"
      VMSymbol s [] -> ":" ++ s
      VMSymbol "list" fields -> showNestedList fields
      VMSymbol s fields ->  ":" ++ s ++ " " ++ (foldr (++) "" $ intersperse " " $ map showField fields)



