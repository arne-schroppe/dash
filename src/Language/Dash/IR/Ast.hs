module Language.Dash.IR.Ast (
  Pattern (..)
, Binding (..)
, Expr (..)
) where


data Pattern
    = PatNumber Int
    | PatVar String
    | PatSymbol String [Pattern]
    | PatWildcard
    deriving (Show, Eq)


data Binding
    = Binding String Expr        -- name, body
    deriving (Show, Eq)


data Expr
    = LitNumber Int
    | LitString String
    | LitSymbol String [Expr]
    | Var String
    | Namespace String Expr  -- TODO merge with var?
    | Lambda [String] Expr       -- arguments, body
    | MatchBranch [String] Expr  -- arguments, body
    | FunAp Expr [Expr]            -- A function application
    | LocalBinding Binding Expr  -- binding, body
    | Module [Binding]
    | Match Expr [(Pattern, Expr)]
    deriving (Show, Eq)



