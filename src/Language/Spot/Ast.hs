module Language.Spot.Ast where

data Pattern
    = PatNumber Int
    | PatVar String
    | PatSymbol (String, [Pattern])
    deriving (Show, Eq)


data Binding 
    = Binding (String, Expr)
    deriving (Show, Eq)


data Expr
    = LitNumber Int
    | LitString String
    | LitSymbol String
    | Var String
    | Namespace (String, Expr)
    | FunDef [String] Expr          -- arguments, body
    | FunCall (Expr, [Expr])
    | LocalBinding (Binding, Expr)  -- binding, body
    | Module [Binding]
    | PatternList [(Pattern, Expr)]
    deriving (Show, Eq)



