module Main where

import Control.Monad
import Test.HUnit
import qualified Data.Text as T

import qualified AstParseTests
import qualified EvalTests
import qualified ParseTests

main :: IO ()
main = runTestTTAndExit $ test
    [ AstParseTests.tests
    , EvalTests.tests
    , ParseTests.tests
    ]
