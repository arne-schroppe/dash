{
module Language.Dash.Parser.Lexer where

import Control.Monad
import Language.Dash.BuiltIn.BuiltInDefinitions (bifStringConcatOperator, bifToStringName)

}

%wrapper "monadUserState"

$newl       = [\n\r]
$alphanum   = [a-zA-Z0-9'_]
$alpha      = [a-zA-Z_]
$digit      = [0-9]
$space      = [\ \t]
$endline    = [\; $newl]
$opsymbol   = [\+ \- \* \/ \$ \# \! \< \> \? \~ \& \| \^]


@ident      = $alpha (\- $alphanum+)? ( ($alphanum+ \-)* $alphanum+ )?
@namespace  = @ident \.

@integer    = "-"? $digit $digit*

@operator   = "==" | $opsymbol ($opsymbol | "=")*

@hashBang   = "#!" .* \n

@stringchars = [^ \"]*



tokens :-
  <0> $space+       ;
  <0> "/--"         { begin mcom }
  <mcom> (. | \n)   ;
  <mcom> "--/"      { begin 0 }

  <0> "--" .*       ;
  <0> ($space* $endline $newl* $space*)+
                    { mkTok TEOL }
  <0> "("           { mkTok TOpen_Par }
  <0> ")"           { mkTok TClose_Par }
  <0> "["           { mkTok TOpen_Bracket }
  <0> "]"           { mkTok TClose_Bracket }
  <0> "module"      { mkTok TModule }
  <0> "if"          { mkTok TIf }
  <0> "then"        { mkTok TThen }
  <0> "else"        { mkTok TElse }
  <0> "match"       { mkTok TMatch }
  <0> "do"          { mkTok TDo }
  <0> "with"        { mkTok TWith }
  <0> "end"         { mkTok TEnd }
  <0> ":" @ident    { mkTokS (\s -> TSymbol (tail s)) }
  <0> "="           { mkTok TDefine }
  <0> "->"          { mkTok TArrow_R }
  <0> "<-"          { mkTok TArrow_L }
  <0> "_"           { mkTok TUnderscore }
  <0> ","           { mkTok TComma }
  <0> "|"           { mkTok TVBar }
  <0> \"\"          { mkTok $ TString "" } -- "
  <0> \"            { begin str } -- "
  <str> @stringchars
                    { mkTokS (\s -> convertEscapeSequences s) }
  <str> \"          { begin 0 } -- "
  <0> @integer      { mkTokS (\s -> TInt (read s)) }
  <0> @namespace    { mkTokS (\s -> TNamespace (init s)) }
  <0> @ident        { mkTokS (\s -> TId s) }
  <0> @operator     { mkTokS (\s -> TOperator s) }
  <0> @hashBang     { skip }


{


mkTok :: Token -> AlexInput -> Int -> Alex Token
mkTok t _ _ = return t

mkTokS :: (String -> Token) -> AlexInput -> Int -> Alex Token
mkTokS f (_, _, _, str) len = return $ f (take len str)

alexEOF :: Alex Token
alexEOF = return TEOF


convertEscapeSequences :: String -> Token
convertEscapeSequences s =
  let parts = conv' s "" [] in
  case parts of
    [InterpString s] -> TString s
    _                -> TRawString parts
  where
    conv' (c:rest@(c1:cs)) acc parts =
      case c of
        '\\' -> parseEscapeChar c1 cs acc parts
        _ -> conv' rest (acc ++ [c]) parts
    conv' (c:[]) acc parts = let lastStringPart = acc ++ [c] in
                             parts ++ [InterpString lastStringPart]
    conv' [] acc parts = parts ++ [InterpString acc]

    parseEscapeChar ec cs acc parts =
      case ec of
        '\\' -> conv' cs (acc ++ "\\") parts -- "
        'n' -> conv' cs (acc ++ "\n") parts
        '"' -> conv' cs (acc ++ "\"") parts
        't' -> conv' cs (acc ++ "\t") parts
        '(' ->
                let (interpExpr, rest) = consumeInterpolatedExpression cs in
                if null rest
                  then parts ++ [InterpString acc, InterpExpr interpExpr]
                  else conv' rest "" $ parts ++ [InterpString acc, InterpExpr interpExpr]
        other -> conv' cs (acc ++ [other]) parts

    consumeInterpolatedExpression str =
      let consume acc rest nparen =
            case rest of
              "" -> error ("Malformed string interpolation: " ++ str)  -- TODO add better error handling
              ch:rest ->
                    case ch of
                      '(' -> consume (acc ++ "(") rest (nparen + 1)
                      ')' -> if nparen == 1
                               then (acc, rest)
                               else consume (acc ++ ")") rest (nparen - 1)
                      _   -> consume (acc ++ [ch]) rest nparen
      in
      consume "" str 1


data AlexUserState = AlexUserState {
  last_token :: Token,
  has_emitted_final_eol :: Bool
}

getHasEmittedEol :: Alex Bool
getHasEmittedEol = Alex $ \st@AlexState{alex_ust = ust} -> Right (st, has_emitted_final_eol ust)

setHasEmittedEol :: Bool -> Alex ()
setHasEmittedEol b = Alex $ \st -> Right (st{alex_ust = (alex_ust st){has_emitted_final_eol = b}}, ())

getPosition :: Alex Int
getPosition = Alex $ \st@AlexState{alex_pos = AlexPn pos _ _} -> Right (st, pos)

getLastToken :: Alex Token
getLastToken = Alex $ \st@AlexState{alex_ust = ust} -> Right (st, last_token ust)

setLastToken :: Token -> Alex ()
setLastToken t = Alex $ \st -> Right (st{alex_ust = (alex_ust st){last_token = t}}, ())


alexInitUserState = AlexUserState {
  has_emitted_final_eol = False,
  last_token = TEOF
}

data InterpStringPart = InterpString String
                      | InterpExpr   String
  deriving (Show, Eq)

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
            | TRawString [InterpStringPart]  -- For internal use only!!!
            | TInt Int
            | TMatch
            | TDo
            | TArrow_R
            | TArrow_L
            | TWith
            | TBegin
            | TEnd
            | TOperator String
            | TUnderscore
            | TComma
            | TVBar
            | TNamespace String
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
              Right a -> expandRawStrings a
              Left s -> error s


expandRawStrings :: [Token] -> [Token]
expandRawStrings tokens =
  case tokens of
    [] -> []
    (TRawString parts):ts ->
        let expanded = [TOpen_Par] ++ (expandRaw parts) ++ [TClose_Par] in
        expanded ++ (expandRawStrings ts)
    t:ts -> t : (expandRawStrings ts)

  where
    expandRaw parts =
      case parts of
        [] -> []
        (InterpString s):[] ->
            [TString s]
        (InterpString s):rest ->
            [TString s, TOperator bifStringConcatOperator] ++ expandRaw rest
        (InterpExpr s):[] ->
            wrapInterpExpr $ lexInterpString s
        (InterpExpr s):rest ->
            let tokens = lexInterpString s in
            wrapInterpExpr tokens ++ [TOperator bifStringConcatOperator] ++ expandRaw rest

    lexInterpString s =
      let tokens = Language.Dash.Parser.Lexer.lex s in
      init tokens -- remove final EOL

    wrapInterpExpr tokens = 
      [TOpen_Par, TId bifToStringName, TOpen_Par] ++ tokens ++ [TClose_Par, TClose_Par]
}

