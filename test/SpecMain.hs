import Spec
import Test.Hspec
import Test.Hspec.Core.Runner
import Test.Hspec.Formatters

main = hspecWith (defaultConfig{configColorMode=ColorAlways, configFormatter=Just specdoc}) spec
