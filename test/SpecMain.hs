import Spec
import Test.Hspec
import Test.Hspec.Core.Runner

main = hspecWith (defaultConfig{configColorMode=ColorAlways}) spec
