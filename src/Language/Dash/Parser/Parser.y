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
  eof       { TEOF }
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
    star(eol) ExprOrEnd star(eol) eof { $2 }


Expr:
    ExprB { $1 }
  | ExprB '=' opt(eol) Expr eol ExprOrEnd {
      case $1 of
        Var s -> LocalBinding (Binding s $4) $6
        _     -> DestructAssignment $1 $4 $6
  }
  | ExprB plus(ExprB) { FunAp $1 $2 }
  | ExprB star(ExprB) '->' opt(eol) Expr { Lambda ($1:$2) $5 }
  | ExprB plus(ExprB) '=' opt(eol) Expr eol ExprOrEnd { LocalBinding (Binding (varName $1) (Lambda $2 $5)) $7 }
  | InfixOperation { $1 }
  | MatchExpr { $1 }
  | DoExpr    { $1 }
  | Module    { $1 }
  | IfElse    { $1 }

ExprOrEnd:
    Expr { $1 }
  |      { LitSymbol "true" [] }

ExprB:
    Ident  { $1 }
  | NonIdentNonSymbolSimpleExpr { $1 }
  | CompoundOrSimpleSymbol { $1 }
  | '_' { Wildcard }


SimpleExpr:
    Ident               { $1 }
  | NonIdentSimpleExpr  { $1 }
  | '_' { Wildcard }


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


-- DestructAssignment:
--    let DestructAssignmentPattern '=' opt(eol) Expr eol Expr { DestructAssignment $2 $5 $7 } -- TODO don't allow patnumber


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
    id plus(ExprB) '=' opt(eol) Expr eol  { Binding $1 (Lambda $2 $5) }


IfElse:
    if opt(eol) Expr opt(eol) then opt(eol) Expr opt(eol) else opt(eol) Expr  {
      Match $3 [(LitSymbol trueSymbolName [], $7), (LitSymbol falseSymbolName [], $11)]
    }


MatchExpr:
    -- TODO also allow indentation syntax
    match Expr with opt(eol) plus(MatchLine) end { Match $2 $5 }

MatchLine:
    Pattern '->' opt(eol) Expr eol { ($1, $4) }

Pattern:
    NonSymbolSimplePattern { $1 }
  | SymbolPattern { $1 }

DestructAssignmentPattern:
    DestructAssignmentSimplePattern { $1 }
  | ComplexSymbolPattern { $1 }

NonSymbolSimplePattern:
    int { LitNumber $1 }
  | PatId { $1 }
  | DestructAssignmentSimplePattern { $1 }

DestructAssignmentSimplePattern:
    '(' Pattern star(TupleNextPattern) ')' { 
      case $3 of
        [] -> $2
        _  -> LitSymbol tupleSymbolName ($2 : $3)
    }
  | PatList  { $1 }
  | PatRecord { $1 }

SimplePattern:
    NonSymbolSimplePattern { $1 }
  | PatPlainSymbol { $1 }

PatId:
    id  { Var $1 }
  | '_' { Wildcard }

PatPlainSymbol:
    symbol  { LitSymbol $1 [] }

TupleNextPattern:
    ',' Pattern   { $2 }

SymbolPattern:
    symbol               { LitSymbol $1 [] }
  | ComplexSymbolPattern { $1 }

ComplexSymbolPattern:
    symbol '<' SymbolPatternNext '>' { LitSymbol $1 $3 }

SymbolPatternNext:
    SimplePattern                        { [$1] }
  | SimplePattern ',' SymbolPatternNext  { $1 : $3 }

PatList:
    '[' PatListNext ']'  { $2 }

PatListNext:
    Pattern                  { LitSymbol listConsSymbolName [$1, LitSymbol listEmptySymbolName []] }
  | Pattern ',' PatListNext  { LitSymbol listConsSymbolName [$1, $3] }
  | Pattern '|' PatId        { LitSymbol listConsSymbolName [$1, $3] }
  | Pattern '|' PatList      { LitSymbol listConsSymbolName [$1, $3] }
  |                          { LitSymbol listEmptySymbolName [] }

PatRecord:
    '{' PatRecordBody '}'  { makeRecordSymbol LitSymbol (\ (LitSymbol a _) -> a) $2 }

PatRecordBody:
    PatRecordEntry star(PatRecordNext) { $1 : $2 }

PatRecordNext:
    ',' PatRecordEntry { $2 }

PatRecordEntry:
    id '=' Pattern  { (LitSymbol $1 [], $3) }


DoExpr:
    do id DoBody  { makeMonad $2 $3 }

DoBody:
    with opt(eol) plus(DoLine) end { $3 }

DoLine:
  -- TODO allow destructuring bind here
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

-- TODO the goal is not to use varName
varName :: Expr -> String
varName (Var s) = s
varName _       = error "expected identifier"

makeMonad :: String -> [(String, Expr)] -> Expr
makeMonad monad lines =
  case (reverse lines) of
    (_, call) : []     -> adjustNameForMonad call monad
    ("_", action) : rest ->
            foldl (\acc (varname, action') ->
                let qname = (Qualified monad $ Var "bind") in
                let args = [(adjustNameForMonad action' monad), Lambda [Var varname] acc] in
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

