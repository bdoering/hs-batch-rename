import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
-- import Utils
import MainCSV

newBasenameTest :: Assertion
newBasenameTest = ["ab", "cd"] @=? basenameTest ["ab", "cd"] ["", ""]

main :: IO ()
main = defaultMainWithOpts
       [ testCase "newBasename" newBasenameTest
       ]
       mempty
