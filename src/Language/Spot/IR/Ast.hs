module Language.Spot.IR.Ast where


data Pattern
    = PatNumber Int
    | PatVar String
    | PatSymbol String [Pattern]
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
    | FunDef [String] Expr       -- arguments, body
    | FunCall Expr [Expr]
    | LocalBinding Binding Expr  -- binding, body
    | Module [Binding]
    | Match Expr [(Pattern, Expr)]
    deriving (Show, Eq)



