{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module AstParseTests
    ( tests
    ) where

import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Test.HUnit
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import AstParse
import TestUtil
import Types

tests :: Test
tests = "ast" ~: TestList
    [ "reference" ~: "x" ??= Ref "x"

    , "literal" ~: TestList
        [ "bool"    ~: mkB True       ??= Lit (mkB True)
        , "number"  ~: 42             ??= Lit 42
        , "char"    ~: Char 'a'       ??= Lit (Char 'a')
        , "string"  ~: String "hello" ??= Lit (String "hello")
        ]

    , "quote" ~: TestList
        [ q 3             ??= Lit 3
        , q ["a", 2]      ??= Lit ["a", 2]
        , ["quote"]       ??! "expected list of length 1"
        , ["quote", 1, 2] ??! "expected list of length 1"
        ]

    , "call" ~: TestList
        [ ["f"]         ??= Call (Ref "f") []
        , ["f", 1, "x"] ??= Call (Ref "f") [Lit 1, Ref "x"]
        ]

    , "lambda" ~: TestList
        [ "pos args" ~:
            ["lambda", ["x", "y"], ["+", "x", "y"]] ??=
            Lambda (Formals ["x", "y"] Nothing)
                   (Body [] (Call (Ref "+") [Ref "x", Ref "y"] :| []))
        , "rest arg" ~:
            ["lambda", "x", "x"] ??=
            Lambda (Formals [] (Just "x"))
                   (Body [] (Ref "x" :| []))
        , "pos + rest args" ~:
            ["lambda", "x" :. "xs", ["cons", "x", "xs"]] ??=
            Lambda (Formals ["x"] (Just "xs"))
                   (Body [] (Call (Ref "cons")
                                  [Ref "x", Ref "xs"] :| []))
        , ["lambda"]      ??! "expected non-empty list"
        , ["lambda", [1]] ??! "expected symbol"
        ]

    , "if" ~: TestList
        [ "else" ~:
            ["if", "x", q "t", q "f"] ??=
            If (Ref "x")
               (Lit "t")
               (Just (Lit "f"))
        , "no else" ~:
            ["if", "x", q "t"] ??=
            If (Ref "x")
               (Lit "t")
               Nothing
        , ["if"]                     ??! "expected list of length 2 or list of length 3"
        , ["if", "a"]                ??! "expected list of length 2 or list of length 3"
        , ["if", "a", "b", "c", "d"] ??! "expected list of length 2 or list of length 3"
        ]

    , "cond" ~: TestList
        [ "else" ~:
            ["cond", [1, 2, 3], ["else", 4]] ??=
            If (Lit 1)
               (Call (Lambda (Formals [] Nothing)
                             (Body [] (Lit 2 :| [Lit 3]))) [])
               (Just (Lit 4))
        , "no else" ~:
            ["cond", [1, 2, 3], [4, 5]] ??=
            If (Lit 1)
               (Call (Lambda (Formals [] Nothing)
                             (Body [] (Lit 2 :| [Lit 3]))) [])
               (Just (If (Lit 4)
                         (Lit 5)
                         Nothing))
        , "arrow" ~:
            ["cond", [1, "=>", 2]] ??=
            Call (Lambda (Formals ["temp"] Nothing)
                         (Body [] (If (Ref "temp")
                                      (Call (Lit 2) [Ref "temp"])
                                      Nothing :| [])))
                 [Lit 1]
        , "only test" ~:
            ["cond", [1]] ??= Lit 1
        , ["cond"]                   ??! "expected non-empty list"
        , ["cond", []]               ??! "expected non-empty list"
        , ["cond", ["else"]]         ??! "expected non-empty list"
        , ["cond", ["else", 1], [1]] ??! "else must be the last clause"
        ]

    , "case" ~: TestList
        [ "else" ~:
            ["case", "x", [["y"], 2, 3], ["else", 3]] ??=
            If (Call (Ref "member")
                     [Ref "x", Lit ["y"]])
               (Call (Lambda (Formals [] Nothing)
                             (Body [] (Lit 2 :| [Lit 3])))
                     [])
               (Just (Lit 3))
        , "no else" ~:
            ["case", "x", [[2, 3], 4], [["y"], 5]] ??=
            If (Call (Ref "member")
                     [Ref "x", Lit [2, 3]]) (Lit 4)
               (Just (If (Call (Ref "member")
                               [Ref "x", Lit ["y"]])
                         (Lit 5)
                         Nothing))
        , "non atom key" ~:
            ["case", ["foo", 1], [[2, 3], 4]] ??=
            Call (Lambda (Formals ["atom-key"] Nothing)
                         (Body [] (If (Call (Ref "member")
                                            [Ref "atom-key", Lit [2, 3]])
                                      (Lit 4)
                                      Nothing :| [])))
                 [Call (Ref "foo") [Lit 1]]
        , "empty" ~:
            ["case", "x", [[], 1]] ??=
            If (Call (Ref "member")
                     [Ref "x", Lit []])
               (Lit 1)
               Nothing
        , ["case"]                ??! "expected list of length at least 2"
        , ["case", "x"]           ??! "expected list of length at least 2"
        , ["case", "x", []]       ??! "expected non-empty list"
        , ["case", "x", ["else"]] ??! "expected non-empty list"
        , ["case", "x", [["y"]]]  ??! "expected non-empty list"
        ]

    , "and" ~: TestList
        [ ["and"] ??= Lit (mkB True)
        , ["and", "x", 1, q "y"] ??=
            If (Ref "x")
               (If (Lit 1)
                   (Lit "y")
                   (Just (Lit (mkB False))))
               (Just (Lit (mkB False)))
        ]

    , "or" ~: TestList
        [ ["or"] ??= Lit (mkB False)
        , ["or", "x", 1, q "y"] ??=
            Call (Lambda (Formals ["x"] Nothing)
                         (Body [] (If (Ref "x")
                                      (Ref "x")
                                      (Just (Call (Lambda (Formals ["x"] Nothing)
                                                          (Body [] (If (Ref "x")
                                                                       (Ref "x")
                                                                       (Just (Lit "y")) :| [])))
                                                  [Lit 1])) :| [])))
                 [Ref "x"]
        ]

    , "let" ~: TestList
        [ "let" ~:
            ["let", [["x", 9], ["y", 5]], "x"] ??=
            Call (Lambda (Formals ["x", "y"] Nothing)
                         (Body [] (Ref "x" :| [])))
                 [Lit 9, Lit 5]
        , "let no vars one expr" ~:
            ["let", [], "x"] ??= Ref "x"
        , "let*" ~:
            ["let*", [["x", 9], ["y", 5]], "x"] ??=
            Call (Lambda (Formals ["x"] Nothing)
                         (Body [] (Call (Lambda (Formals ["y"] Nothing)
                                                (Body [] (Ref "x" :| [])))
                                        [Lit 5] :| [])))
                 [Lit 9]
        , "letrec" ~:
            ["letrec", [["x", 9], ["y", 5]], "x"] ??=
            Letrec [("x", Lit 9), ("y", Lit 5)]
                   (Body [] (Ref "x" :| []))
        , ["let"]                ??! "expected non-empty list"
        , ["let", [["x", 1, 2]]] ??! "expected list of length 2"
        , ["let", [["x", 1]]]    ??! "expected non-empty list"
        , ["let", [[1, 1]]]      ??! "expected symbol"
        ]

    , "begin" ~: TestList
        [ "one" ~:
            ["begin", "x"] ??= Ref "x"
        , "many" ~:
            ["begin", "x", "y"] ??=
            Call (Lambda (Formals [] Nothing)
                         (Body [] (Ref "x" :| [Ref "y"])))
                 []
        , ["begin"] ??! "expected non-empty list"
        ]

    , "do" ~: TestList
        [ "step" ~:
            ["do", [["x", 5, 9]],
                   ["test", "foo"],
                ["bar", "x"]] ??=
            Letrec [("do-loop", Lambda (Formals ["x"] Nothing)
                                       (Body [] (If (Ref "test")
                                                    (Ref "foo")
                                                    (Just (Call (Lambda (Formals [] Nothing)
                                                                        (Body [] (Call (Ref "bar")
                                                                                       [Ref "x"] :|
                                                                                 [Call (Ref "do-loop")
                                                                                       [Lit 9]])))
                                                                [])) :| [])))]
                   (Body [] (Call (Ref "do-loop") [Lit 5] :| []))
        , "no step" ~:
            ["do", [["x", 5]],
                   ["test", "foo"],
                ["bar", "x"]] ??=
            Letrec [("do-loop", Lambda (Formals ["x"] Nothing)
                                       (Body [] (If (Ref "test")
                                                    (Ref "foo")
                                                    (Just (Call (Lambda (Formals [] Nothing)
                                                                        (Body [] (Call (Ref "bar")
                                                                                       [Ref "x"] :|
                                                                                 [Call (Ref "do-loop")
                                                                                       [Ref "x"]])))
                                                                            [])) :| [])))]
                   (Body [] (Call (Ref "do-loop") [Lit 5] :| []))
        , "no test exprs" ~:
            ["do", [["x", 5, 9]],
                   ["test"],
                ["bar", "x"]] ??=
            Letrec [("do-loop", Lambda (Formals ["x"] Nothing)
                                       (Body [] (If (Ref "test")
                                                    (Lit Unspec)
                                                    (Just (Call (Lambda (Formals [] Nothing)
                                                                        (Body [] (Call (Ref "bar")
                                                                                       [Ref "x"] :|
                                                                                 [Call (Ref "do-loop")
                                                                                       [Lit 9]])))
                                                                            [])) :| [])))]
                   (Body [] (Call (Ref "do-loop") [Lit 5] :| []))
        , "no commands" ~:
            ["do", [["x", 5, 9]],
                   ["test", "foo"]] ??=
            Letrec [("do-loop", Lambda (Formals ["x"] Nothing)
                                       (Body [] (If (Ref "test")
                                                    (Ref "foo")
                                                    (Just (Call (Ref "do-loop")
                                                                [Lit 9])) :| [])))]
                   (Body [] (Call (Ref "do-loop") [Lit 5] :| []))
        , "no inits" ~:
            ["do", [], ["test"]] ??=
            Letrec [("do-loop", Lambda (Formals [] Nothing)
                                       (Body [] (If (Ref "test")
                                                    (Lit Unspec)
                                                    (Just (Call (Ref "do-loop") [])) :| [])))]
                   (Body [] (Call (Ref "do-loop") [] :| []))
        , ["do"]                             ??! "expected list of length at least 2"
        , ["do", [["x"]], ["test"]]          ??! "expected list of length 2 or list of length 3"
        , ["do", [["x", 1, 1, 1]], ["test"]] ??! "expected list of length 2 or list of length 3"
        , ["do", [[1, 1]], ["test"]]         ??! "expected symbol"
        , ["do", [["x", 1]], []]             ??! "expected non-empty list"
        ]

    , "named let" ~: TestList
        [ "init" ~:
            ["let", "loop", [["x", 1]], ["loop", "x"]] ??=
            Letrec [("loop", Lambda (Formals ["x"] Nothing)
                                    (Body [] (Call (Ref "loop") [Ref "x"] :| [])))]
                   (Body [] (Call (Ref "loop") [Lit 1] :| []))
        , "no inits" ~:
            ["let", "loop", [], "foo"] ??=
            Letrec [("loop", Lambda (Formals [] Nothing)
                                    (Body [] (Ref "foo" :| [])))]
                   (Body [] (Call (Ref "loop") [] :| []))
        , ["let", "loop", []] ??! "expected non-empty list"
        ]

    , "delay" ~: TestList
        [ ["delay", "x"]      ??= Delay (Ref "x")
        , ["delay"]           ??! "expected list of length 1"
        , ["delay", "x", "y"] ??! "expected list of length 1"
        ]

    , "define" ~: TestList
        [ "let with val define" ~:
            ["let", [], ["define", "x", 1], "x"] ??=
            Call (Lambda (Formals [] Nothing)
                         (Body [("x", Lit 1)] (Ref "x" :| [])))
                 []

        , "let with procedure define" ~:
            ["let", [], ["define", ["succ", "x"], ["+", 1, "x"]], ["succ", "x"]] ??=
            Call (Lambda (Formals [] Nothing)
                         (Body [("succ", Lambda (Formals ["x"] Nothing)
                                                (Body [] (Call (Ref "+")
                                                               [Lit 1, Ref "x"] :| [])))]
                               (Call (Ref "succ") [Ref "x"] :| [])))
                 []

        , "lambda with val define" ~:
            ["lambda", [], ["define", "x", 1], "x"] ??=
            Lambda (Formals [] Nothing)
                   (Body [("x", Lit 1)]
                         (Ref "x" :| []))
        ]
    ]

infixl 2 ??=
(??=) :: SVal -> Expr -> Test
x ??= e = test $ case pExpr x of
    Left pf  -> assertFailure $ T.unpack $ parseFailMsg pf :: IO ()
    Right e' -> e' @?= e

infixl 2 ??!
(??!) :: SVal -> String -> Test
x ??! e = test $ case pExpr x of
    Left pf ->
        let msg = T.unpack $ parseFailMsg pf
        in e `isInfixOf` msg @? (m ++ ", but got " ++ msg)
    Right _  -> assertFailure (m ++ ", but succeeded")
  where
    m = "expected fail with: " ++ e
