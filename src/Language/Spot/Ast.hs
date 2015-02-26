module Language.Spot.Ast where

data Pattern
    = PatNumber Int
    | PatVar String
    | PatSymbol (String, [Pattern])

data Binding 
    = Binding (String, Expr)

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



