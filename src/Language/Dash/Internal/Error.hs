module Language.Dash.Internal.Error (
  CompilationError(..)
) where


-- TODO one constructor per error + show instance?
data CompilationError =
    InternalCompilerError String
  | CodeError String


