module Language.Dash.Error.Error (
  CompilationError(..)
) where


-- TODO one constructor per error + show instance?
data CompilationError =
    ParsingError String
  | InternalCompilerError String
  | CodeError String
  deriving (Eq)

instance Show CompilationError where
  show err =
    case err of
      InternalCompilerError msg -> "Internal compiler error: " ++ msg
      ParsingError msg -> "Parser error: " ++ msg
      CodeError msg -> "Error: " ++ msg


