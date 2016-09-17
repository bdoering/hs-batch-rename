import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
-- import Utils
import MainCSV hiding (main)

newBasenameTest1 = ["ab", "cd"] @=? newBasename ["ab", "cd"] ["", ""]
newBasenameTest2 = ["ab", ""]   @=? newBasename ["", ""]     ["ab", ""]
newBasenameTest3 = ["ab", "ef"] @=? newBasename ["ab", "cd"] ["", "ef"]
newBasenameTest4 = ["ab", "cd", ""] @=?
                   newBasename ["ab", "cd", "ef"] ["", "#"]

main :: IO ()
main = defaultMainWithOpts
       [ testCase "newBasename1" newBasenameTest1
       , testCase "newBasename2" newBasenameTest2
       , testCase "newBasename3" newBasenameTest3
       , testCase "newBasename4" newBasenameTest4
       ]
       mempty
