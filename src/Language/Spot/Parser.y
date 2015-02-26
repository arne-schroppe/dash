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

Val   : val     { Var "a" }



{


parseError :: [Token] -> a
parseError _ = error "Parse error"


}
