{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module ParseTests
    ( tests
    ) where

import Data.List
import Test.HUnit
import qualified Data.Text as T

import Parse
import TestUtil
import Types

tests :: Test
tests = "parser" ~: TestList
    [ "bool" ~: TestList
        [ "#f" ??= mkB False
        , "#t" ??= mkB True
        ]
    , "symbol" ~: TestList
        [ "hello123" ??= "hello123"
        , "a->b"     ??= "a->b"
        ]
    , "number" ~: TestList
        [ "42"       ??= 42
        , "-42"      ??= (-42)
        , "#b101010" ??= 42
        , "#o52"     ??= 42
        , "#d42"     ??= 42
        , "#x2a"     ??= 42
        ]
    , "char" ~: TestList
        [ "#\\a"       ??= Char 'a'
        , "#\\space"   ??= Char ' '
        , "#\\newline" ??= Char '\n'
        ]
    , "string" ~: TestList
        [ "\"hello\""             ??= String "hello"
        , "\"double quote \\\"\"" ??= String "double quote \""
        , "\"backslash \\\\\""    ??= String "backslash \\"
        , "\"newline \\n\""       ??= String "newline \n"
        , "\"tab \\t\""           ??= String "tab \t"
        , "\"incomplete"          ??! "expecting '\"'"
        ]
    , "list" ~: TestList
        [ "()"       ??= Nil
        , "(#t a 2)" ??= [mkB True, "a", 2]
        ]
    , "improper list" ~: TestList
        [ "(1 b . 3)" ??= 1 :. "b" :. 3
        ]
    , "vector" ~: TestList
        [ "#()"        ??= Vector []
        , "#(a 1 b 2)" ??= Vector ["a", 1, "b", 2]
        ]
    , "quoted" ~: TestList
        [ "'(a b 'c)" ??= q ["a", "b", q "c"]
        , "' a"       ??= q "a"
        ]
    , "comment" ~: TestList
        [ ";comment\n42;comment\n;comment" ??= 42
        ]
    , "complex" ~: TestList
        [ "(define (fib n) (if (<= 1 n) n (+ (fib (- n 1)) (fib (- n 2)))))" ??=
            ["define", ["fib", "n"],
                ["if", ["<=", 1, "n"],
                       "n",
                       ["+", ["fib", ["-", "n", 1]],
                             ["fib", ["-", "n", 2]]]]]
        ]
    ]

infixl 2 ??=
(??=) :: T.Text -> SVal -> Test
s ??= e = test $ case readManyPrettyErr "<test>" s of
    Left msg -> assertFailure msg
    Right a  -> a @?= [e]

infixl 2 ??!
(??!) :: T.Text -> String -> Test
s ??! e = test $ case readManyPrettyErr "<test>" s of
    Left msg -> e `isInfixOf` msg @? (m ++ ", but got " ++ msg)
    Right _  -> assertFailure (m ++ ", but succeeded")
  where
    m = "expected fail with: " ++ e
