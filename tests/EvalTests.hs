{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module EvalTests
    ( tests
    ) where

import Control.Monad.Catch
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Test.HUnit

import Eval
import Run
import TestUtil
import Types

tests :: Test
tests = "eval" ~: TestList
    [ "lit" ~: TestList
        [ Lit (mkB True)       ??= mkB True
        , Lit 1                ??= 1
        , Lit "foo"            ??= "foo"
        , Lit ["a", "list", 2] ??= ["a", "list", 2]
        ]

    , "lambda/call/ref" ~: TestList
        [ Call (Lambda (Formals [] Nothing)
                       (Body [] (Lit 1 :| [])))
               [] ??=
            1
        , Call (Lambda (Formals ["x"] Nothing)
                       (Body [] (Ref "x" :| [])))
               [Lit 1] ??=
            1
        , Call (Lambda (Formals ["x", "y"] Nothing)
                       (Body [] (Ref "y" :| [])))
               [Lit 1, Lit "s"] ??=
            "s"
        , Call (Lambda (Formals [] (Just "x"))
                       (Body [] (Ref "x" :| [])))
               [Lit 1, Lit "s"] ??=
            [1, "s"]

        , Call (Lambda (Formals ["x"] Nothing)
                       (Body [] (Lit 1 :| [])))
               [] ??!
            "expected list of length 1"
        , Call (Lambda (Formals ["x"] Nothing)
                       (Body [] (Lit 1 :| [])))
               [Lit 1, Lit 2] ??!
            "expected list of length 1"
        , Call (Lambda (Formals ["x"] (Just "y"))
                       (Body [] (Lit 1 :| [])))
               [] ??!
            "expected list of length at least 1"
        ]

    , "if" ~: TestList
        [ If (Lit (mkB True)) (Lit 1) (Just (Lit 2))  ??= 1
        , If (Lit (mkB False)) (Lit 1) (Just (Lit 2)) ??= 2
        , If (Lit (mkB True)) (Lit 1) Nothing         ??= 1
        , If (Lit (mkB False)) (Lit 1) Nothing        ??= Unspec
        , If (Lit 5) (Lit 1) Nothing                  ??= 1
        , If (Lit Nil) (Lit 1) Nothing                ??= 1
        ]

    , "letrec" ~: TestList
        [ Letrec [("x", Lit 1)] (Body [] (Ref "x" :| [])) ??= 1
        , Letrec [("fact", Lambda (Formals ["n"] Nothing)
                                  (Body [] (If (Call (Ref "=") [Ref "n", Lit 0])
                                               (Lit 1)
                                               (Just (Call (Ref "*")
                                                           [Ref "n", Call (Ref "fact")
                                                                          [Call (Ref "-")
                                                                                [Ref "n", Lit 1]]])) :| [])))]
                 (Body [] (Call (Ref "fact") [Lit 6] :| [])) ??=
            720
        , Letrec [("x", Lit 1), ("y", Ref "x")] (Body [] (Lit 1 :| [])) ??!
            "cyclic evaluation in fixIO"
        ]

      -- Can't really test the behavior of delay, at least not very easily
    , "delay" ~: TestList
        [ Call (Ref "force") [Delay (Lit 1)] ??= 1
        ]
    ]

infixl 2 ??=
(??=) :: Expr -> SVal -> Test
e ??= x = test $ interpS (runTopLevel (TopLevel [] [e])) >>= (x @=?)

infixl 2 ??!
(??!) :: Expr -> String -> Test
e ??! s = test $ catchAll
    (interpS (runTopLevel (TopLevel [] [e])) *> assertFailure (m ++ ", but succeeded"))
    ((\msg -> s `isInfixOf` msg @? m ++ ", but got " ++ msg) . displayException)
  where               
    m = "expected fail with: " ++ s
