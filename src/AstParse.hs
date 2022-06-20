{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Parse Scheme values into Scheme ASTs.
module AstParse
    ( ParseFail(..)
    , pExpr
    , pTopLevel
    ) where

import Control.Monad
import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T

import Conv
import SValOverloads ()
import Types
import Util

-- | A parse failure.
newtype ParseFail = ParseFail { parseFailMsg :: T.Text }

-- | Parses multiple values into a 'TopLevel'.
pTopLevel :: [SVal] -> Either ParseFail TopLevel
pTopLevel ((Symbol "define" :. rest) : xs) = dcons <$> pDefine rest <*> pTopLevel xs
  where
    dcons d (TopLevel ds xs') = TopLevel (d:ds) xs'
pTopLevel xs = TopLevel [] <$> traverse pExpr xs

-- | Parses a value into an expression.
pExpr :: SVal -> Either ParseFail Expr
pExpr = \case
    Nil      -> Left $ ParseFail "empty application"
    Symbol s -> pure $ Ref s
    Symbol s :. xs
        | Just f <- M.lookup s primitiveExprParsers -> f xs
        | Just f <- M.lookup s derivedExprParsers   -> f xs >>= pExpr
    f :. xs -> pCall f xs
    x       -> pure $ Lit x

primitiveExprParsers :: M.HashMap T.Text (SVal -> Either ParseFail Expr)
primitiveExprParsers = M.fromList
    [ ("quote",  pQuote)
    , ("lambda", pLambda)
    , ("if",     pIf)
    , ("letrec", pLetrec)
    , ("delay",  pDelay)
    ]

derivedExprParsers :: M.HashMap T.Text (SVal -> Either ParseFail SVal)
derivedExprParsers = M.fromList
    [ ("cond",  pCond)
    , ("case",  pCase)
    , ("and",   pAnd)
    , ("or",    pOr)
    , ("let",   pLet)
    , ("let*",  pLetStar)
    , ("begin", pBegin)
    , ("do",    pDo)
    ]

-- | Ensures argument names are distinct.
mkFormals :: [T.Text] -> Maybe T.Text -> Either ParseFail Formals
mkFormals names name
    | distinct (maybe id (:) name names) = pure $ Formals names name
    | otherwise                          = Left $ ParseFail "duplicate variable name"

-- | Ensures variable names are distinct.
mkLetrec :: [VarInit] -> Body -> Either ParseFail Expr
mkLetrec varInits body
    | distinct (map fst varInits) = pure $ Letrec varInits body
    | otherwise                   = Left $ ParseFail "duplicate variable name"

convE :: Conv a -> SVal -> Either ParseFail a
convE = first (ParseFail . convFailMsg) .: runConv

pCall :: SVal -> SVal -> Either ParseFail Expr
pCall f args = Call <$> pExpr f <*> (convE cList args >>= traverse pExpr)

pQuote :: SVal -> Either ParseFail Expr
pQuote = fmap Lit . convE cList1

pLambda :: SVal -> Either ParseFail Expr
pLambda = convE cNonEmpty >=> \(formalsList :| bodyList) ->
    Lambda <$> (convE cFormalsList formalsList >>= toFormals) <*> pBody (mkSList bodyList)
  where
    cFormalsList = (E3A <$> cSymbol) <> (E3B <$> cList) <> (E3C <$> cImproperList)
    toFormals = \case
        E3A s  -> mkFormals [] (Just s)
        E3B xs -> join $ mkFormals <$> traverse (convE cSymbol) xs <*> pure Nothing
        E3C (xs, x) -> join $
            mkFormals <$> traverse (convE cSymbol) (NE.toList xs) <*> (Just <$> convE cSymbol x)

pIf :: SVal -> Either ParseFail Expr
pIf = convE ((Left <$> cList2) <> (Right <$> cList3)) >=> \case
    Left  (test, conseq)      -> If <$> pExpr test <*> pExpr conseq <*> pure Nothing
    Right (test, conseq, alt) -> If <$> pExpr test <*> pExpr conseq <*> (Just <$> pExpr alt)

pCond :: SVal -> Either ParseFail SVal
pCond = convE cNonEmpty >=> go
  where
    go = \case
        ("else" :. xs) :| [] -> pure $ "begin" :. xs
        ("else" :. _)  :| _  -> Left $ ParseFail "else must be the last clause"
        c :| cs -> goTest <$> convE cNonEmpty c <*> maybe (pure Unspec) go (nonEmpty cs)
    goTest (t :| []) Unspec = t
    goTest (t :| xs) else_  = case xs of
        []        -> ["let", [["temp", t]], "if" :. "temp" :. "temp" :. end']
        ["=>", p] -> ["let", [["temp", t]], "if" :. "temp" :. [p, "temp"] :. end']
        _         -> "if" :. t :. ("begin" :. mkSList xs) :. end'
      where
        end' = case else_ of Unspec -> []; _ -> [else_]

pCase :: SVal -> Either ParseFail SVal
pCase = convE cNonEmpty2 >=> go
  where
    go (k@(_ :. _), xs) = (\x -> ["let", [["atom-key", k]], x]) <$> go ("atom-key", xs)
    go (k, xs)          = go' xs
      where
        go' = \case
            ("else" :. xs') :| [] -> pure $ "begin" :. xs'
            ("else" :. _)   :| _  -> Left $ ParseFail "else must be the last clause"
            c :| cs -> goMatch <$> convE cNonEmpty c <*> maybe (pure Unspec) go' (nonEmpty cs)
        goMatch (ds :| xs') else_ =
            let end' = case else_ of Unspec -> []; _ -> [else_]
            in "if" :. ["member", k, ["quote", ds]] :. ("begin" :. mkSList xs') :. end'

pAnd :: SVal -> Either ParseFail SVal
pAnd = fmap go . convE cList
  where
    go []     = Boolean True
    go [x]    = x
    go (x:xs) = ["if", x, go xs, Boolean False]

pOr :: SVal -> Either ParseFail SVal
pOr = fmap go . convE cList
  where
    go []     = Boolean False
    go [x]    = x
    go (x:xs) = ["let", [["x", x]], ["if", "x", "x", go xs]]

pLet :: SVal -> Either ParseFail SVal
pLet e@(Symbol _ :. _) = pNamedLet e
pLet e                 = pPlainLet e

pPlainLet :: SVal -> Either ParseFail SVal
pPlainLet = convE cNonEmpty >=> \(varInits :| body) ->
    convE cList varInits >>= traverse (convE cList2) >>= pure . \case
        [] -> "begin" :. mkSList body
        varInits' ->
            ("lambda" :. mkSList (map fst varInits') :. mkSList body) :. mkSList (map snd varInits')

pLetStar :: SVal -> Either ParseFail SVal
pLetStar = convE cNonEmpty >=> go
  where
    go (varInits :| body) = convE cList varInits >>= go'
      where
        go' [] = pure $ "begin" :. mkSList body
        go' [varInit] = do
            (var, init') <- convE cList2 varInit
            pure $ "let" :. [[var, init']] :. mkSList body
        go' (varInit:varInits') = do
            (var, init') <- convE cList2 varInit
            body' <- go' varInits'
            pure ["let", [[var, init']], body']

pLetrec :: SVal -> Either ParseFail Expr
pLetrec = convE cNonEmpty >=> \(varInits :| body) ->
    join $ mkLetrec <$>
        (convE cList varInits >>= traverse pVarInit) <*>
        pBody (mkSList body)
  where
    pVarInit = convE cList2 >=> \(var, init') -> (,) <$> convE cSymbol var <*> pExpr init'

pBegin :: SVal -> Either ParseFail SVal
pBegin = pure . \case
    x :. Nil -> x
    xs       -> ["lambda" :. [] :. xs]

pDo :: SVal -> Either ParseFail SVal
pDo = convE cNonEmpty2 >=> \(varInitSteps, test :| cmds) -> do
    varInitSteps' <- convE cList varInitSteps >>=
        traverse (fmap padStep . convE ((Left <$> cList2) <> (Right <$> cList3)))
    test' :| exprs <- convE cNonEmpty test
    let lam = ["lambda", mkSList (map fst3 varInitSteps'),
            ["if", test',
                   if null exprs then Unspec else "begin" :. mkSList exprs,
                   "begin" :. mkSList (cmds ++ ["do-loop" :. mkSList (map thd3 varInitSteps')])]]
    pure ["letrec", [["do-loop", lam]], "do-loop" :. mkSList (map snd3 varInitSteps')]
  where
    padStep (Left (var, init'))        = (var, init', var)
    padStep (Right (var, init', step)) = (var, init', step)

pNamedLet :: SVal -> Either ParseFail SVal
pNamedLet = convE cNonEmpty2 >=> \(name, varInits :| body) -> do
    varInits' <- convE cList varInits >>= traverse (convE cList2)
    let lam = "lambda" :. mkSList (map fst varInits') :. mkSList body
    pure ["letrec", [[name, lam]], name :. mkSList (map snd varInits')]

pDelay :: SVal -> Either ParseFail Expr
pDelay = convE cList1 >=> fmap Delay . pExpr

pDefine :: SVal -> Either ParseFail VarInit
pDefine = convE cNonEmpty >=> \(d :| rest) -> convE cFormalsList d >>= \case
    E3A s -> convE cList1 (mkSList rest) >>= fmap (s,) . pExpr
    E3B (x :| xs) ->
        (,) <$>
            convE cSymbol x <*>
            (Lambda <$>
                join (mkFormals <$> traverse (convE cSymbol) xs <*> pure Nothing) <*>
                pBody (mkSList rest))
    E3C (x :| xs, x') ->
        (,) <$>
            convE cSymbol x <*>
            (Lambda <$>
                join (mkFormals <$> traverse (convE cSymbol) xs <*> (Just <$> convE cSymbol x')) <*>
                pBody (mkSList rest))
  where
    cFormalsList = (E3A <$> cSymbol) <> (E3B <$> cNonEmpty) <> (E3C <$> cImproperList)

pBody :: SVal -> Either ParseFail Body
pBody ((Symbol "define" :. rest) :. xs) = dcons <$> pDefine rest <*> pBody xs
  where
    dcons d (Body ds xs') = Body (d:ds) xs'
pBody xs = convE cNonEmpty xs >>= fmap (Body []) . traverse pExpr
