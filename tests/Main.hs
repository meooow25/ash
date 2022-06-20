module Main where

import Control.Monad
import Test.HUnit
import qualified Data.Text as T

import qualified AstParseTests
import qualified ParseTests

main :: IO ()
main = runTestTTAndExit $ test
    [ AstParseTests.tests
    , ParseTests.tests
    ]
