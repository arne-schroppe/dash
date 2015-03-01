{
  module Language.Spot.Lexer where
}

%wrapper "monadUserState"

$newl       = [\n\r]
$alphanum   = [a-zA-Z0-9]
$alpha      = [a-zA-Z]
$digit      = [0-9]
$space      = [\ \t]

@ident      = $alpha( ($alphanum+ \-)* $alphanum+ )?
@namespaces = (@ident \/)*

@integer    = $digit $digit*



tokens :-
<0>  $space+       ;
<0>  "/--"
      (. | \n)*
      "--/"        ;
<0>  "--" .*       ;
<0>  $newl+        { mkTok TEOL }
<0>  "("           { mkTok TOpen_Par }
<0>  ")"           { mkTok TClose_Par }
<0>  "val"         { mkTok TVal }
<0>  "module"      { mkTok TModule }
<0>  "match"       { mkTok TMatch }
<0>  "do"          { mkTok TDo }
<0>  ":" @ident    { mkTokS (\s -> TSymbol (tail s)) }
<0>  "="           { mkTok TEqual }
<0>  "->"          { mkTok TArrow_R }
<0>  "<-"          { mkTok TArrow_L }
<0>  "with"        { mkTok TWith }
<0>  @integer      { mkTokS (\s -> TInt (read s)) }
<0>  @ident        { mkTokS (\s -> TId s) }
<0>  eof           { mkTok TEOF }




{

mkTok :: Token -> AlexInput -> Int -> Alex Token
mkTok t _ _ = return t

mkTokS :: (String -> Token) -> AlexInput -> Int -> Alex Token
mkTokS f (_, _, _, str) len = return $ f (take len str)

alexEOF :: Alex Token
alexEOF = return TEOF

data AlexUserState = AlexUserState {
  has_emitted_final_eol :: Bool
}

alexInitUserState = AlexUserState { has_emitted_final_eol = False }

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


loop = do
  t <- alexMonadScan
  if (t == TEOF)
      then return []
      else do toks <- loop
              return (t : toks)


lex :: String -> [Token]
lex input = case (runAlex input loop) of
              Right a -> a
              Left s -> error s


}

