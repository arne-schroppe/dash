{
  module Language.Spot.Lexer where
}

%wrapper "basic"

$newl    = [\n]


tokens :-
  $newl+          { \s -> EOL }


{

data Token = EOL
           | EOF

  deriving (Show, Eq)


lex = alexScanTokens


}

