{
module Language.Dash.Parser.Lexer (
  lex
, Token(..)
) where

import Prelude                                   hiding (lex)
import Language.Dash.BuiltIn.BuiltInDefinitions (bifStringConcatOperator, bifToStringName)
import Language.Dash.Error.Error (CompilationError(..))

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
  <0> "{"           { mkTok TOpen_Brace }
  <0> "}"           { mkTok TClose_Brace }
  <0> "module"      { mkTok TModule }
  <0> "if"          { mkTok TIf }
  <0> "then"        { mkTok TThen }
  <0> "else"        { mkTok TElse }
  <0> "match"       { mkTok TMatch }
  <0> "do"          { mkTok TDo }
  <0> "with"        { mkTok TWith }
  <0> "end"         { mkTok TEnd }
  <0> ":" @ident    { mkTokS (\s -> return $ TSymbol (tail s)) }
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
  <0> @integer      { mkTokS (\s -> return $ TInt (read s)) }
  <0> @namespace    { mkTokS (\s -> return $ TNamespace (init s)) }
  <0> @ident        { mkTokS (\s -> return $ TId s) }
  <0> @operator     { mkTokS (\s -> return $ TOperator s) }
  <0> @hashBang     { skip }


{


lex :: String -> Either CompilationError [Token]
lex input = case (runAlex input loop) of
              Right a -> expandRawStrings a
              Left s -> Left $ ParsingError s


mkTok :: Token -> AlexInput -> Int -> Alex Token
mkTok t _ _ = return t

mkTokS :: (String -> Either CompilationError Token) -> AlexInput -> Int -> Alex Token
mkTokS f (_, _, _, str) len =
  case f (take len str) of
    Left err -> alexError $ show err
    Right token -> return token

alexEOF :: Alex Token
alexEOF = return TEOF


convertEscapeSequences :: String -> Either CompilationError Token
convertEscapeSequences s = do
  parts <- conv' s "" []
  case parts of
    [InterpString s] -> return $ TString s
    _                -> return $ TRawString parts
  where

    conv' :: String -> String -> [InterpStringPart] -> Either CompilationError [InterpStringPart]
    conv' (c:rest@(c1:cs)) acc parts =
      case c of
        '\\' -> parseEscapeChar c1 cs acc parts
        _ -> conv' rest (acc ++ [c]) parts
    conv' (c:[]) acc parts = let lastStringPart = acc ++ [c] in
                             return $ parts ++ [InterpString lastStringPart]
    conv' [] acc parts = return $ parts ++ [InterpString acc]

    parseEscapeChar ec cs acc parts =
      case ec of
        '\\' -> conv' cs (acc ++ "\\") parts -- "
        'n' -> conv' cs (acc ++ "\n") parts
        '"' -> conv' cs (acc ++ "\"") parts
        't' -> conv' cs (acc ++ "\t") parts
        '(' -> do
                  (interpExpr, rest) <- consumeInterpolatedExpression cs
                  if null rest
                    then return $ parts ++ [InterpString acc, InterpExpr interpExpr]
                    else conv' rest "" $ parts ++ [InterpString acc, InterpExpr interpExpr]
        other -> conv' cs (acc ++ [other]) parts

    consumeInterpolatedExpression :: String -> Either CompilationError (String, String)
    consumeInterpolatedExpression str =
      let consume acc rest nparen =
            case rest of
              "" -> Left $ ParsingError ("Malformed string interpolation: " ++ str)  -- TODO add better error handling
              ch:rest ->
                    case ch of
                      '(' -> consume (acc ++ "(") rest (nparen + 1)
                      ')' -> if nparen == 1
                               then Right $ (acc, rest)
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
            | TOpen_Brace
            | TClose_Brace
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
                                     return [TEOF]
        else do setHasEmittedEol True
                return [TEOL, TEOF]




expandRawStrings :: [Token] -> Either CompilationError [Token]
expandRawStrings tokens =
  case tokens of
    [] -> return []
    (TRawString parts):ts -> do expandedParts <- expandRaw parts
                                let expanded = [TOpen_Par] ++ expandedParts ++ [TClose_Par]
                                expandedRest <- expandRawStrings ts
                                return $ expanded ++ expandedRest
    t:ts -> do expandedRest <- expandRawStrings ts
               return $ t : expandedRest

  where
    expandRaw :: [InterpStringPart] -> Either CompilationError [Token]
    expandRaw parts =
      case parts of
        [] -> 
            return []
        (InterpString s):[] ->
            return [TString s]
        (InterpString s):rest ->
            do expandedRest <- expandRaw rest
               return $ [TString s, TOperator bifStringConcatOperator] ++ expandedRest
        (InterpExpr s):[] ->
            do lexed <- lexInterpString s
               return $ wrapInterpExpr lexed
        (InterpExpr s):rest ->
            do tokens <- lexInterpString s
               let wrapped = wrapInterpExpr tokens
               expandedRest <- expandRaw rest
               return $ wrapped ++ [TOperator bifStringConcatOperator] ++ expandedRest

    lexInterpString :: String -> Either CompilationError [Token]
    lexInterpString s =
      do tokens <- Language.Dash.Parser.Lexer.lex s
         return $ (init . init) tokens -- remove final EOL, EOF

    wrapInterpExpr :: [Token] -> [Token]
    wrapInterpExpr tokens =
      [TOpen_Par, TId bifToStringName, TOpen_Par] ++ tokens ++ [TClose_Par, TClose_Par]
}

