{
module Language.Dash.Parser.Parser where

import Language.Dash.Parser.Lexer
import Language.Dash.IR.Ast


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
  module    { TModule }
  if        { TIf }
  then      { TThen }
  else      { TElse }
  '='       { TDefine }
  symbol    { TSymbol $$ }
  id        { TId $$ }
  -- TODO qid       { TQId $$ }
  string    { TString $$ }
  int       { TInt $$ }
  match     { TMatch }
  do        { TDo }
  '->'      { TArrow_R }
  '<-'      { TArrow_L }
  -- with      { TWith }
  begin     { TBegin }
  end       { TEnd }
  lam       { TLambda }
  operator  { TOperator $$ }
  '_'       { TUnderscore }
  ','       { TComma }
  '|'       { TVBar }


%%

-- TODO for some reason we can't have a comment as our last element in a file

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
    opt(eol) Expr star(eol) { $2 }
  | star(eol)               { LitString "" }



Expr:
    Ident          { $1 }
  | NonIdentNonSymbolSimpleExpr { $1 }
  | LocalBinding   { $1 }
  | FunDefOrAp     { $1 }
  | Lambda         { $1 }
  | MatchExpr      { $1 }
  | DoExpr         { $1 }
  | Module         { $1 }
  | CompoundOrSimpleSymbol  { $1 }
  | InfixOperation   { $1 }
  | IfElse         { $1 }


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
  | '(' Expr star(TupleNextExpr) ')' {
                  case $3 of
                  [] -> $2
                  es -> LitSymbol tupleSymbolId ($2 : $3) }

TupleNextExpr:
    ',' Expr   { $2 }



FunDefOrAp:
    NonIdentNonSymbolSimpleExpr plus(SimpleExpr)       { FunAp $1 $2 }
  | Ident NonIdentSimpleExpr star(SimpleExpr) { FunAp $1 ($2 : $3)  }
  | Ident Ident FunDefOrCallNext              { let args = $2 : (fst $3) in (snd $3) $1 args }


FunDefOrCallNext:
    Ident FunDefOrCallNext                     { ($1 : (fst $2), (snd $2)) } -- could still be a fun def or application
  | NonIdentSimpleExpr star(SimpleExpr)        { ($1 : $2, \ a args -> FunAp a args)   }  -- application
  | '=' opt(eol) Expr eol Expr                 { let varName (Var vn) = vn in
                                                 ([], \ a args ->
                                                        LocalBinding (Binding (varName a) (Lambda (map varName args) $3)) $5) } -- fun def
  |                                            { ([], \ a args -> FunAp a args) }


InfixOperation:
    SimpleExpr operator SimpleExpr             { FunAp (Var $2) [$1, $3] }


List:
    '[' ListNext ']'       { $2 }

ListNext:
    Expr              { LitSymbol "list" [$1, LitSymbol "empty-list" []] }
  | Expr ',' ListNext { LitSymbol "list" [$1, $3] }
  |                   { LitSymbol "empty-list" [] }



LocalBinding:
    Binding Expr  { LocalBinding $1 $2 }

Binding:
    id '=' opt(eol) Expr eol   { Binding $1 $4 }


Ident:
    id    { Var $1 }

Lambda:
    lam plus(id) '=' opt(eol) Expr  { Lambda $2 $5 }


CompoundOrSimpleSymbol:
    symbol star(SimpleExpr) { LitSymbol $1 $2 }



Module:
    module opt(eol) star(Definition) end  { Module $3 }

Definition:
    Binding        { $1 }
  | ModuleFunDef   { $1 }

ModuleFunDef:
    id plus(id) '=' opt(eol) Expr eol  { Binding $1 (Lambda $2 $5) }


IfElse:
    if opt(eol) Expr opt(eol) then opt(eol) Expr opt(eol) else opt(eol) Expr  {
      Match $3 [(PatSymbol "true" [], $7), (PatSymbol "false" [], $11)]
    }


MatchExpr:
    -- TODO also allow indentation syntax
    match Expr begin opt(eol) plus(MatchLine) end { Match $2 $5 }

MatchLine:
    Pattern '->' opt(eol) Expr eol { ($1, $4) }

Pattern:
    SimplePattern { $1 }
  | SymbolPattern { $1 }

SimplePattern:
    int { PatNumber $1 }
  | PatId { $1 }
  | '(' Pattern star(TupleNextPattern) ')' { 
      case $3 of
        [] -> $2
        _  -> PatSymbol tupleSymbolId ($2 : $3)
    }
  | PatList  { $1 }

PatId:
    id  { PatVar $1 }
  | '_' { PatWildcard }

TupleNextPattern:
    ',' Pattern   { $2 }

SymbolPattern:
    symbol star(SimplePattern) { PatSymbol $1 $2 }

PatList:
    '[' PatListNext ']'  { $2 }

PatListNext:
    Pattern                  { PatSymbol "list" [$1, PatSymbol "empty-list" []] }
  | Pattern ',' PatListNext  { PatSymbol "list" [$1, $3] }
  | Pattern '|' PatId        { PatSymbol "list" [$1, $3] }
  |                          { PatSymbol "empty-list" [] }


DoExpr:
    do id DoBody  { makeMonad $2 $3 }

DoBody:
    begin opt(eol) plus(DoLine) end eol  { $3 }

DoLine:
    id '<-' DoLineExpr eol  { ($1, $3) }
  | DoLineExpr eol          { ("_", $1) }


DoLineExpr:
    Ident          { $1 }
  | NonIdentSimpleExpr { $1 }
  | LocalDoBinding        { $1 }  
  | MatchExpr     { $1 }
  | FunAp          { $1 }


FunAp:
    SimpleExpr plus(SimpleExpr) { FunAp $1 $2 }

LocalDoBinding:
    Binding DoLineExpr  { LocalBinding $1 $2 }




{

tupleSymbolId = "$tuple"


makeMonad monad lines =
  case (reverse lines) of
    (_, call) : []     -> call
    ("_", call) : rest -> foldl (\acc (nid, ncall) ->
          let ns = (Namespace monad (Var "bind")) in
          let args = [ncall, Lambda [nid] acc] in
          FunAp ns args)
        call rest
    (_, _) : rest -> error "Last line in do-block can't be an assignment"
    [] -> error "Malformed do-block"


parseError :: [Token] -> a
parseError ts = error $ "Parse error " ++ (show ts)


}
