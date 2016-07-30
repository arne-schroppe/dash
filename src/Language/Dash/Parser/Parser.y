{
module Language.Dash.Parser.Parser where

import Language.Dash.Parser.Lexer
import Language.Dash.IR.Ast
import Language.Dash.BuiltIn.BuiltInDefinitions

import Data.List (sortBy)

}

%name       parse
%tokentype  { Token }
%error      { parseError }

%token
  eol       { TEOL }
  '('       { TOpen_Par }
  ')'       { TClose_Par }
  '['       { TOpen_Bracket }
  ']'       { TClose_Bracket }
  '{'       { TOpen_Brace }
  '}'       { TClose_Brace }
  module    { TModule }
  if        { TIf }
  then      { TThen }
  else      { TElse }
  let       { TLet }
  '='       { TDefine }
  symbol    { TSymbol $$ }
  id        { TId $$ }
  ns        { TNamespace $$ }
  -- TODO qid       { TQId $$ }
  string    { TString $$ }
  int       { TInt $$ }
  match     { TMatch }
  do        { TDo }
  '->'      { TArrow_R }
  '<-'      { TArrow_L }
  with      { TWith }
  -- begin     { TBegin }
  end       { TEnd }
  '+'       { TOperator "+" }
  '-'       { TOperator "-" }
  '/'       { TOperator "/" }
  '*'       { TOperator "*" }
  '=='      { TOperator "==" }
  '<'       { TOperator "<" }
  '>'       { TOperator ">" }
  '<='      { TOperator "<=" }
  '>='      { TOperator ">=" }
  '^+'      { TOperator "^+" }
  '++'      { TOperator "++" }
  '||'      { TOperator "||" }
  '&&'      { TOperator "&&" }
  '!'       { TOperator "!" }
  operator  { TOperator $$ }
  '_'       { TUnderscore }
  ','       { TComma }
  '|'       { TVBar }


%left '||' 
%left '&&'
%left '==' '<' '>' '<=' '>='
%left '+' '-' '^+' '++'
%left '*' '/'
%left NEG '!'

%%


-- TODO distinguish between eol and sep (separator)
-- TODO also, this parser will not be able to handle a mix of eol and sep


opt(p):
    p                   { Just $1 }
  |                     { Nothing }


star(p):
  p star(p)       { $1 : $2 }
  |               { [] }

plus(p):
  p star(p)       { $1 : $2 }



Prog:
    star(eol) Expr star(eol) { $2 }
  | star(eol)               { LitString "" }



Expr:
    Ident          { $1 }
  | NonIdentNonSymbolSimpleExpr { $1 }
  | LocalBinding   { $1 }
  | FunDefOrApOrLambda { $1 }
  | MatchExpr      { $1 }
  | DoExpr         { $1 }
  | Module         { $1 }
  | CompoundOrSimpleSymbol  { $1 }
  | InfixOperation { $1 }
  | IfElse         { $1 }
  | DestructAssignment  { $1 }


SimpleExpr:
    Ident               { $1 }
  | NonIdentSimpleExpr  { $1 }


NonIdentSimpleExpr:
    symbol         { LitSymbol $1 [] }
  | NonIdentNonSymbolSimpleExpr { $1 }

NonIdentNonSymbolSimpleExpr:
    int            { LitNumber $1 }
  | string         { LitString $1 }
  | List           { $1 }
  | Record         { $1 }
  | '(' Expr star(TupleNextExpr) ')' {
                  case $3 of
                  [] -> $2
                  es -> LitSymbol tupleSymbolName ($2 : $3) }

TupleNextExpr:
    ',' Expr   { $2 }



FunDefOrApOrLambda:
    NonIdentNonSymbolSimpleExpr plus(SimpleExpr) { FunAp $1 $2 }
  | Ident NonIdentSimpleExpr star(SimpleExpr) { FunAp $1 ($2 : $3)  }
  | Ident Ident FunDefOrCallOrLambdaNext      { let args = $2 : (fst $3) in (snd $3) $1 args }
  | Ident '->' opt(eol) Expr                  { Lambda [varName $1] $4 }


FunDefOrCallOrLambdaNext:
    Ident FunDefOrCallOrLambdaNext             { ($1 : (fst $2), (snd $2)) } -- could still be a fun def or application
  | NonIdentSimpleExpr star(SimpleExpr)        { ($1 : $2, \ a args -> FunAp a args)   }  -- application
  | '=' opt(eol) Expr eol Expr                 { ([], \ a args ->
                                                        LocalBinding (Binding (varName a) (Lambda (map varName args) $3)) $5) } -- fun def
  | '->' opt(eol) Expr                         { ([], \ a args -> Lambda (map varName (a : args)) $3) }
  |                                            { ([], \ a args -> FunAp a args) }



InfixOperation:
    Operand '+' Operand         { FunAp (Var "+") [$1, $3] }
  | Operand '-' Operand         { FunAp (Var "-") [$1, $3] }
  | Operand '/' Operand         { FunAp (Var "/") [$1, $3] }
  | Operand '*' Operand         { FunAp (Var "*") [$1, $3] }
  | Operand '==' Operand        { FunAp (Var "==") [$1, $3] }
  | Operand '<' Operand         { FunAp (Var "<") [$1, $3] }
  | Operand '>' Operand         { FunAp (Var ">") [$1, $3] }
  | Operand '^+' Operand        { FunAp (Var bifStringConcatName) [$1, $3] }
  | Operand '++' Operand        { FunAp (Var bifListConcatName) [$1, $3] }
  | Operand '||' Operand        { FunAp (Var "||") [$1, $3] }
  | Operand '&&' Operand        { FunAp (Var "&&") [$1, $3] }
  | Operand '<=' Operand        { FunAp (Var "<=") [$1, $3] }
  | Operand '>=' Operand        { FunAp (Var ">=") [$1, $3] }
  | '!' Operand                 { FunAp (Var "!") [$2] }
  | '-' Operand %prec NEG       { FunAp (Var "-") [LitNumber 0, $2] }
  -- | Operand operator Operand    { FunAp (Var $2) [$1, $3] }

Operand:
    SimpleExpr     { $1 }
  | InfixOperation { $1 }



List:
    '[' ListNext ']'       { $2 }

ListNext:
    Expr              { LitSymbol listConsSymbolName [$1, LitSymbol listEmptySymbolName []] }
  | Expr ',' ListNext { LitSymbol listConsSymbolName [$1, $3] }
  | Expr '|' Expr     { LitSymbol listConsSymbolName [$1, $3] }
  |                   { LitSymbol listEmptySymbolName [] }


Record:
    '{' opt(eol) RecordBody '}'  { makeRecordSymbol LitSymbol (\ (LitSymbol a _) -> a) $3 }

RecordBody:
    RecordEntry star(RecordNext) { $1 : $2 }

RecordNext:
    ',' opt(eol) RecordEntry { $3 }

RecordEntry:
    id opt(eol) '=' Expr opt(eol)  { (LitSymbol $1 [], $4) }



LocalBinding:
    Binding Expr  { LocalBinding $1 $2 }

Binding:
    id '=' opt(eol) Expr eol   { Binding $1 $4 }


Ident:
    id    { Var $1 }
  | ns Ident  { Qualified $1 $2 } -- TODO use namespace


CompoundOrSimpleSymbol:
    symbol '<' SymbolNext '>' { LitSymbol $1 $3 }
  | symbol { LitSymbol $1 [] }

SymbolNext:
    SimpleExpr                { [$1] }
  | SimpleExpr ',' SymbolNext { $1 : $3 }


Module:
    module opt(eol) star(Definition) end  { Module $3 }

Definition:
    Binding        { $1 }
  | ModuleFunDef   { $1 }

ModuleFunDef:
    id plus(id) '=' opt(eol) Expr eol  { Binding $1 (Lambda $2 $5) }


IfElse:
    if opt(eol) Expr opt(eol) then opt(eol) Expr opt(eol) else opt(eol) Expr  {
      Match $3 [(PatSymbol trueSymbolName [], $7), (PatSymbol falseSymbolName [], $11)]
    }

DestructAssignment:
    let Pattern '=' opt(eol) Expr eol Expr { DestructAssignment $2 $5 $7 } -- TODO don't allow patnumber

MatchExpr:
    -- TODO also allow indentation syntax
    match Expr with opt(eol) plus(MatchLine) end { Match $2 $5 }

MatchLine:
    Pattern '->' opt(eol) Expr eol { ($1, $4) }

Pattern:
    NonSymbolSimplePattern { $1 }
  | SymbolPattern { $1 }

NonSymbolSimplePattern:
    int { PatNumber $1 }
  | PatId { $1 }
  | '(' Pattern star(TupleNextPattern) ')' { 
      case $3 of
        [] -> $2
        _  -> PatSymbol tupleSymbolName ($2 : $3)
    }
  | PatList  { $1 }
  | PatRecord { $1 }

SimplePattern:
    NonSymbolSimplePattern { $1 }
  | PatPlainSymbol { $1 }

PatId:
    id  { PatVar $1 }
  | '_' { PatWildcard }

PatPlainSymbol:
    symbol  { PatSymbol $1 [] }

TupleNextPattern:
    ',' Pattern   { $2 }

SymbolPattern:
    symbol                     { PatSymbol $1 [] }
  | symbol '<' SymbolPatternNext '>' { PatSymbol $1 $3 }

SymbolPatternNext:
    SimplePattern                        { [$1] }
  | SimplePattern ',' SymbolPatternNext  { $1 : $3 }

PatList:
    '[' PatListNext ']'  { $2 }

PatListNext:
    Pattern                  { PatSymbol listConsSymbolName [$1, PatSymbol listEmptySymbolName []] }
  | Pattern ',' PatListNext  { PatSymbol listConsSymbolName [$1, $3] }
  | Pattern '|' PatId        { PatSymbol listConsSymbolName [$1, $3] }
  | Pattern '|' PatList      { PatSymbol listConsSymbolName [$1, $3] }
  |                          { PatSymbol listEmptySymbolName [] }

PatRecord:
    '{' PatRecordBody '}'  { makeRecordSymbol PatSymbol (\ (PatSymbol a _) -> a) $2 }

PatRecordBody:
    PatRecordEntry star(PatRecordNext) { $1 : $2 }

PatRecordNext:
    ',' PatRecordEntry { $2 }

PatRecordEntry:
    id '=' Pattern  { (PatSymbol $1 [], $3) }


DoExpr:
    do id DoBody  { makeMonad $2 $3 }

DoBody:
    with opt(eol) plus(DoLine) end { $3 }

DoLine:
    id '<-' DoLineExpr eol  { ($1, $3) }
  | DoLineExpr eol          { ("_", $1) }
  | Binding DoLine          { let (v, e) = $2 in
                              (v, LocalBinding $1 e) }

DoLineExpr:
    Ident              { $1 }
  | NonIdentSimpleExpr { $1 }
  | MatchExpr          { $1 }
  | IfElse             { $1 }
  | FunAp              { $1 }
  | DoExpr             { $1 }


FunAp:
    SimpleExpr plus(SimpleExpr) { FunAp $1 $2 }


{

varName :: Expr -> String
varName (Var vn) = vn

makeMonad :: String -> [(String, Expr)] -> Expr
makeMonad monad lines =
  case (reverse lines) of
    (_, call) : []     -> adjustNameForMonad call monad
    ("_", action) : rest ->
            foldl (\acc (varname, action') ->
                let qname = (Qualified monad $ Var "bind") in
                let args = [(adjustNameForMonad action' monad), Lambda [varname] acc] in
                FunAp qname args) (adjustNameForMonad action monad) rest
    (_, _) : rest -> error "Last line in do-block can't be an assignment"
    [] -> error "Malformed do-block"


adjustNameForMonad :: Expr -> String -> Expr
adjustNameForMonad e mon =
  case e of
    FunAp (Var "return") a -> FunAp (Qualified mon $ Var "return") a
    LocalBinding b e -> LocalBinding b (adjustNameForMonad e mon)
    _ -> e

parseError :: [Token] -> a
parseError ts = error $ "Parse error " ++ (show ts)



makeRecordSymbol :: (String -> [a] -> a) -> (a -> String) -> [(a, a)] -> a
makeRecordSymbol symbolCtor keyName kvs = symbolCtor recordSymbolName body
  where
    body = concat $ map (\(a, b) -> [a, b]) sortedKvs
    sortedKvs = sortBy (\(a, _) (b, _) -> compare (keyName a) (keyName b)) kvs


}

