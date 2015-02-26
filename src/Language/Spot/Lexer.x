{
  module Language.Spot.Lexer where
}

%wrapper "basic"

$newl       = [\n\r]
$alphanum   = [a-zA-Z0-9]
$alpha      = [a-zA-Z]
$digit      = [0-9]
$space      = [\ \t]

@ident      = $alpha( ($alphanum+ \-)* $alphanum+ )?
@namespaces = (@ident \/)*

@integer    = $digit $digit*



tokens :-
  $space+         ;
  "/--" (. | \n)* "--/"
                  ;
  "--" .*         ;
  $newl+          { \s -> TEOL }
  "("             { \s -> TOpen_Par }
  ")"             { \s -> TClose_Par }
  "val"           { \s -> TVal }
  "module"        { \s -> TModule }
  "match"         { \s -> TMatch }
  "do"            { \s -> TDo }
  ":" @ident      { \s -> TSymbol (tail s) }
  "="             { \s -> TEqual }
  "->"            { \s -> TArrow_R }
  "<-"            { \s -> TArrow_L }
  "with"          { \s -> TWith }
  @integer        { \s -> TInt (read s) }
  @ident          { \s -> TId s }




{

data Token  = TEOL
            | TEOF
            | TOpen_Par
            | TClose_Par
            | TVal
            | TModule
            | TWith
            | TEqual
            | TSymbol String
            | TId String
            | TQId ([String], String)
            | TString String
            | TInt Int
            | TSemicolon
            | TMatch
            | TDo
            | TArrow_R
            | TArrow_L
            | TIndent
            | TOutdent
  deriving (Show, Eq)


lex :: String -> [Token]
lex = alexScanTokens


}

