{
module Language.Spot.Parser.Parser where

import Language.Spot.Parser.Lexer
import Language.Spot.Parser.Ast


}

%name       parse
%tokentype  { Token }
%error      { parseError }

%token
  eol       { TEOL }
  eof       { TEOF }
  '('       { TOpen_Par }
  ')'       { TClose_Par }
  val       { TVal }
  module    { TModule }
  with      { TWith }
  '='       { TEqual }
  symbol    { TSymbol $$ }
  id        { TId $$ }
  qid       { TQId $$ }
  string    { TString $$ }
  int       { TInt $$ }
  ';'       { TSemicolon }
  match     { TMatch }
  do        { TDo }
  '->'      { TArrow_R }
  '<-'      { TArrow_L }
  indent    { TIndent }
  outdent   { TOutdent }


%%

opt(p):
    p                   { Just $1 }
  |                     { Nothing }


star(p):
  p star(p)       { $1 : $2 }
  |               { [] }

plus(p):
  p star(p)       { $1 : $2 }


Prog:
    opt(eol) Expr opt(eol)  { $2 }

Expr:
    Local_binding      { $1 }
  | Anon_fun           { $1 }
  | Anon_module        { $1 }
  | Fun_call           { $1 }
  | Match_expr         { $1 }
  | Do_expr            { $1 }
  | Complex_symbol     { $1 }
  | Non_symbol_literal { $1 }
  | Qid_or_parenthesis_expr { $1 }

Simple_expr:
    Literal                 { $1 }
  | Qid_or_parenthesis_expr { $1 }

Qid_or_parenthesis_expr:
    Qid                     { $1 }
  | '(' Expr ')' { $2 }

Local_binding:
    Definition Expr  { LocalBinding $1 $2 }
  | Definition Block_start Expr Block_end  { LocalBinding $1 $3 }


Definition:
    Def_start Def_rest  { Binding $1 $2 }
  | Def_start Fun_rest { Binding $1 $2 }

Def_start:
    val id { $2 }

Def_rest:
    '=' Body { $2 }

Anon_fun:
    val Fun_rest  { $2 }

Fun_rest:
    '(' opt(eol) plus(id) opt(eol) ')' Def_rest  { FunDef $3 $6 }



Body:
    Block_start Expr Block_end { $2 }
  | Expr eol { $1 }

Block_start:
    opt(eol) indent { () }

Block_end:
    opt(eol) outdent { () }

Anon_module:
    module Block_start star(Definition) Block_end  { Module $3 }

Literal:
    Symbol_literal       { $1 }
  | Non_symbol_literal  { $1 }

Non_symbol_literal:
    Number_literal   { $1  }
  | String_literal  { $1 }

Complex_symbol:
    symbol star(Simple_expr) { LitSymbol $1 $2 }

Symbol_literal:
    symbol                   { LitSymbol $1 [] }

Number_literal:
    int              { LitNumber $1 }

String_literal:
    string          { LitString $1 }


Fun_call:
    Qid_or_parenthesis_expr plus(Fun_args) opt(Anon_fun)  { let af' = case $3 of
                                                                Just a  -> [a]
                                                                Nothing -> [] in
                                                            FunCall $1 ($2 ++ af') }

Fun_args:
    Simple_expr          { $1 }

Do_expr:
  {- TODO id should be a qid -}
    do id with Do_body  { makeMonad $2 $4 }

Do_body:
    Block_start plus(Do_line) Block_end  { $2 }

Do_line:
    id '<-' Do_line_expr Line_end  { ($1, $3) }
  | Do_line_expr Line_end          { ("_", $1) }

Do_line_expr:
    Fun_call            { $1 }
  | Qid                 { $1 }
  | Non_symbol_literal  { $1 }
  | Complex_symbol      { $1 }

Match_expr:
    match Simple_expr with Match_body { FunCall (Var "$match") [$4, $2] }

Match_body:
    Block_start plus(Match_line) Block_end  { PatternList $2 }

Match_line:
    Pattern '->' Expr Line_end { ($1, $3) }

Line_end:
    eol  { }
  | ';'  {}

Pattern:
    Simple_pattern { $1 }
  | Symbol_pattern { $1 }

Simple_pattern:
    int { PatNumber $1 }
  | id  { PatVar $1 }
  | '(' Pattern ')' { $2 }

Symbol_pattern:
    symbol star(Simple_pattern) { PatSymbol $1 $2 }

Qid:
    id   { Var $1 }
  | qid { let (nss, id) = $1 in
          foldl (\ e ns -> Namespace ns e) (Var id) (reverse nss)
            }




{

makeMonad monad lines =
  case (reverse lines) of
    (_, call) : []     -> call
    ("_", call) : rest -> foldl (\acc (nid, ncall) ->
          let ns = (Namespace monad (Var "bind")) in
          let args = [ncall, FunDef [nid] acc] in
          FunCall ns args)
        call rest
    (_, _) : rest -> error "Last line in do-block can't be an assignment"
    [] -> error "Malformed do-block"


parseError :: [Token] -> a
parseError ts = error $ "Parse error " ++ (show ts)


}
