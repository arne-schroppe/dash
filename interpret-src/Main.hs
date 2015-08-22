import           Control.Monad
import           System.Environment
import           System.IO

import           Language.Dash.API

main = do
  args <- getArgs
  when ( (length args) == 0) $ error "Expected script path"
  let scriptPath = args !! 0
  fileContent <- readFile scriptPath
  result <- runWithPreamble fileContent
  case result of
    Left err -> print err
    Right value -> print value
