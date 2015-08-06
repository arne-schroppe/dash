{
module Language.Dash.Parser.Lexer where

import Debug.Trace
}

%wrapper "monadUserState"

$newl       = [\n\r]
$alphanum   = [a-zA-Z0-9']
$alpha      = [a-zA-Z]
$digit      = [0-9]
$space      = [\ \t]
$endline    = [\; $newl]
$opsymbol   = [\+ \- \* \/ \$ \# \! \< \> \? \~ \& \^]


@ident      = $alpha( ($alphanum+ \-)* $alphanum+ )?
@namespaces = (@ident \/)*

@integer    = "-"? $digit $digit*

@operator   = "==" | $opsymbol ($opsymbol | "=")*

-- TODO strings


tokens :-
  $space+       ;
  "/--"
   (. | \n)*
   "--/"        ;
  "--" .*       ;
  ($space* $endline $newl* $space*)+
                { mkTok TEOL }
  "("           { mkTok TOpen_Par }
  ")"           { mkTok TClose_Par }
  "["           { mkTok TOpen_Bracket }
  "]"           { mkTok TClose_Bracket }
  "module"      { mkTok TModule }
  "if"          { mkTok TIf }
  "then"        { mkTok TThen }
  "else"        { mkTok TElse }
  "match"       { mkTok TMatch }
  "do"          { mkTok TDo }
  "with"        { mkTok TWith }
  "begin"       { mkTok TBegin }
  "end"         { mkTok TEnd }
  ".\"          { mkTok TLambda }
  ":" @ident    { mkTokS (\s -> TSymbol (tail s)) }
  "="           { mkTok TDefine }
  "->"          { mkTok TArrow_R }
  "<-"          { mkTok TArrow_L }
  "_"           { mkTok TUnderscore }
  ","           { mkTok TComma }
  "|"           { mkTok TVBar }
  @integer      { mkTokS (\s -> TInt (read s)) }
  @ident        { mkTokS (\s -> TId s) }
  @operator     { mkTokS (\s -> TOperator s) }


{

mkTok :: Token -> AlexInput -> Int -> Alex Token
mkTok t _ _ = return t

mkTokS :: (String -> Token) -> AlexInput -> Int -> Alex Token
mkTokS f (_, _, _, str) len = return $ f (take len str)

alexEOF :: Alex Token
alexEOF = return TEOF



data AlexUserState = AlexUserState {
  last_token :: Token,
  has_emitted_final_eol :: Bool
}

getHasEmittedEol :: Alex Bool
getHasEmittedEol = Alex $ \st@AlexState{alex_ust = ust} -> Right (st, has_emitted_final_eol ust)

setHasEmittedEol :: Bool -> Alex ()
setHasEmittedEol b = Alex $ \st -> Right (st{alex_ust = (alex_ust st){has_emitted_final_eol = b}}, ())


getLastToken :: Alex Token
getLastToken = Alex $ \st@AlexState{alex_ust = ust} -> Right (st, last_token ust)

setLastToken :: Token -> Alex ()
setLastToken t = Alex $ \st -> Right (st{alex_ust = (alex_ust st){last_token = t}}, ())


alexInitUserState = AlexUserState {
  has_emitted_final_eol = False,
  last_token = TEOF
}

data Token  = TEOL
            | TEOF
            | TOpen_Par
            | TClose_Par
            | TOpen_Bracket
            | TClose_Bracket
            | TIf
            | TThen
            | TElse
            | TModule
            | TDefine
            | TSymbol String
            | TId String
            -- | TQId ([String], String)
            | TString String
            | TInt Int
            | TMatch
            | TDo
            | TArrow_R
            | TArrow_L
            | TWith
            | TBegin
            | TEnd
            | TLambda
            | TOperator String
            | TUnderscore
            | TComma
            | TVBar
  deriving (Show, Eq)


loop = do
  t <- alexMonadScan
  case t of
    TEOF -> checkFinalEol
    TEOL -> do lt <- getLastToken
               if lt == TEOL      -- we never insert two EOL after another
                  then skipToken
                  else next t
    _    -> next t

next t = do setLastToken t
            toks <- loop
            return (t : toks)

skipToken = do toks <- loop
               return toks

checkFinalEol = do
        e <- getHasEmittedEol
        lt <- getLastToken
        if (e || lt == TEOL) then do setHasEmittedEol True
                                     return []
        else do setHasEmittedEol True
                return [TEOL]


lex :: String -> [Token]
lex input = case (runAlex input loop) of
              Right a -> a
              Left s -> error s


}

