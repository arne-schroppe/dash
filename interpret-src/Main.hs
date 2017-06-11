import           Control.Monad
import           Control.Monad.IO.Class
import           System.Environment
import           System.IO
import           Language.Dash.BuiltIn.BuiltInDefinitions  (preamble)

import           Language.Dash.API

main = do
  args <- getArgs
  when ( (length args) == 0) $ error "Expected script path"
  let scriptPath = args !! 0
  fileContent <- readFile scriptPath

  case (length args, if length args > 1 then args !! 1 else "") of
       (1, _) -> runWithPreamble fileContent >>= showResult
       (2, "--toAsm") -> putStrLn $ showCompiledProgram (preamble ++ fileContent)
       (2, "--toNorm") -> putStrLn $ showNormalizedProgram (preamble ++ fileContent)
       (_, _) -> print "Unexpected command line argument"





showResult result =
  case result of
    Left err -> print err
    Right value -> print value
