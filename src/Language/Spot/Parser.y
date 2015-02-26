{
module Language.Spot.Parser where

import Language.Spot.Lexer
import Language.Spot.Ast

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
  string    { TString $$ }
  int       { TInt $$ }
  ';'       { TSemicolon }
  match     { TMatch }
  do        { TDo }
  '->'      { TArrow_R }
  '<-'      { TArrow_L }


%%

opt(p):
    p                   { Just $1 }
  |                     { Nothing }

Prog:
    opt(eol) Expr opt(eol)  { $2 }

Expr:
    Symbol  { $1 }

Symbol:
    symbol  { LitSymbol $1 }



{


parseError :: [Token] -> a
parseError _ = error "Parse error"


}
