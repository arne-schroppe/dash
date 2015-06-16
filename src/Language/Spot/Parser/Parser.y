{
module Language.Spot.Parser.Parser where

import Language.Spot.Parser.Lexer
import Language.Spot.IR.Ast


}

%name       parse
%tokentype  { Token }
%error      { parseError }

%token
  eol       { TEOL }
  '('       { TOpen_Par }
  ')'       { TClose_Par }
  module    { TModule }
  '='       { TEqual }
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


%%

-- TODO for some reason we can't have a comment as our last element in a file


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



Expr:
    Ident          { $1 }
  | NonIdentNonSymbolSimpleExpr { $1 }
  | LocalBinding   { $1 }
  | FunDefOrAp     { $1 }
  | Lambda         { $1 }
  | MatchExpr     { $1 }
  | DoExpr        { $1 }
  | Module         { $1 }
  | CompoundOrSimpleSymbol  { $1 }


SimpleExpr:
    Ident               { $1 }
  | NonIdentSimpleExpr  { $1 }

NonIdentSimpleExpr:
    int            { LitNumber $1 }
  | symbol         { LitSymbol $1 [] }
  | string         { LitString $1 }
  | '(' Expr ')'   { $2 }

NonIdentNonSymbolSimpleExpr:
    int            { LitNumber $1 }
  | string         { LitString $1 }
  | '(' Expr ')'   { $2 }



FunDefOrAp:
    NonIdentNonSymbolSimpleExpr plus(SimpleExpr)       { FunCall $1 $2 }
  | Ident NonIdentSimpleExpr star(SimpleExpr) { FunCall $1 ($2 : $3)  }
  | Ident Ident FunDefOrCallNext              { let args = $2 : (fst $3) in (snd $3) $1 args }


FunDefOrCallNext:
    Ident FunDefOrCallNext                     { ($1 : (fst $2), (snd $2)) } -- could still be a fun def or a call
  | NonIdentSimpleExpr star(SimpleExpr)        { ($1 : $2, \ a args -> FunCall a args)   }  -- fun call
  | '=' opt(eol) Expr eol Expr                 { let varName (Var vn) = vn in
                                                 ([], \ a args -> 
                                                        LocalBinding (Binding (varName a) (Lambda (map varName args) $3)) $5) } -- fun def
  |                                            { ([], \ a args -> FunCall a args) }





LocalBinding:
    Binding Expr  { LocalBinding $1 $2 }

Binding:
    id '=' opt(eol) Expr eol   { Binding $1 $4 }


Ident:
    id    { Var $1 }

Lambda:
    lam plus(id) '=' Expr  { Lambda $2 $4 }


CompoundOrSimpleSymbol:
    symbol star(SimpleExpr) { LitSymbol $1 $2 }



Module:
    module opt(eol) star(Definition) end  { Module $3 }

Definition:
    Binding        { $1 }
  | ModuleFunDef   { $1 }

ModuleFunDef:
    id plus(id) '=' opt(eol) Expr eol  { Binding $1 (Lambda $2 $5) }



MatchExpr:
    -- TODO also allow indentation syntax
    match Expr begin opt(eol) plus(MatchLine) end { Match $2 $5 }

MatchLine:
    Pattern '->' Expr eol { ($1, $3) }

Pattern:
    SimplePattern { $1 }
  | SymbolPattern { $1 }

SimplePattern:
    int { PatNumber $1 }
  | id  { PatVar $1 }
  | '(' Pattern ')' { $2 }

SymbolPattern:
    symbol star(SimplePattern) { PatSymbol $1 $2 }




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
    SimpleExpr plus(SimpleExpr) { FunCall $1 $2 }

LocalDoBinding:
    Binding DoLineExpr  { LocalBinding $1 $2 }




{

makeMonad monad lines =
  case (reverse lines) of
    (_, call) : []     -> call
    ("_", call) : rest -> foldl (\acc (nid, ncall) ->
          let ns = (Namespace monad (Var "bind")) in
          let args = [ncall, Lambda [nid] acc] in
          FunCall ns args)
        call rest
    (_, _) : rest -> error "Last line in do-block can't be an assignment"
    [] -> error "Malformed do-block"


parseError :: [Token] -> a
parseError ts = error $ "Parse error " ++ (show ts)


}
