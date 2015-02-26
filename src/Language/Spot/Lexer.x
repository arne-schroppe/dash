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
  $newl+          { \s -> EOL }
  "("             { \s -> Open_Par }
  ")"             { \s -> Close_Par }
  "val"           { \s -> Val }
  "module"        { \s -> Module }
  "match"         { \s -> Match }
  "do"            { \s -> Do }
  ":" @ident      { \s -> Symbol (tail s) }
  "="             { \s -> Equal }
  "->"            { \s -> Arrow_R }
  "<-"            { \s -> Arrow_L }
  "with"          { \s -> With }
  @integer        { \s -> Int (read s) }
  @ident          { \s -> Id s }




{

data Token  = EOL
            | EOF
            | Open_Par
            | Close_Par
            | Val
            | Module
            | With
            | Equal
            | Symbol String
            | Id String
            | QId ([String], String)
            | String String
            | Int Int
            | Semicolon
            | Match
            | Do
            | Arrow_R
            | Arrow_L
            | Indent
            | Outdent
  deriving (Show, Eq)


lex :: String -> [Token]
lex = alexScanTokens


}

