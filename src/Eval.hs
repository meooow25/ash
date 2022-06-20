{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Evaluate Scheme ASTs.
module Eval
    ( runTopLevel
    ) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State
import Data.Bool
import Data.List.NonEmpty (nonEmpty)
import qualified Data.HashMap.Strict as M

import Conv
import Types
import Util

-- | Runs a 'TopLevel'. The bindings introduced by it are preserved.
runTopLevel :: TopLevel -> InterpS SVal
runTopLevel (TopLevel varInits xs) = do
    env <- get
    env' <- liftIO $ runInterp (bindVarInits varInits) env
    put env'
    liftIO $ runInterp (maybe (pure Unspec) (traverse1 eval) (nonEmpty xs)) env'

eval :: Expr -> Interp SVal
eval = \case
    Ref s -> asks (M.lookup s) >>= maybe (throwM $ SException $ "unbound variable " <> s) pure

    Lit x -> pure x

    Call f xs -> (eval f >>= convM cProcedure) <*> traverse eval xs >>= liftIO

    Lambda formals body -> do
        env <- ask
        pure $ Procedure $ MkProcedure $
            convM (cListWithLen (formalsCount formals)) . mkSList >=>
            runInterp (evalBody body) . (<> env) . bindArgs formals
      where
        formalsCount (Formals names name) = maybe Exactly (const AtLeast) name $ length names
        bindArgs (Formals names name) args = M.fromList $ case name of
            Nothing -> zip names args
            Just name' ->
                let (namedArgs, restArgs) = splitAt (length names) args
                in zip (name':names) (mkSList restArgs : namedArgs)

    If test conseq alt -> eval test >>= bool (maybe (pure Unspec) eval alt) (eval conseq) . asBool

    Letrec varInits body -> bindVarInits varInits >>= inEnv (evalBody body)

    Delay x -> do
        env <- ask
        liftIO $ Promise <$> mkProm (runInterp (eval x) env)

inEnv :: Interp a -> Env -> Interp a
inEnv = flip $ local . const

convM :: MonadThrow m => Conv a -> SVal -> m a
convM = either (throwM . SException . convFailMsg) pure .: runConv

evalBody :: Body -> Interp SVal
evalBody (Body ds body) = bindVarInits ds >>= inEnv (traverse1 eval body)

bindVarInits :: [VarInit] -> Interp Env
bindVarInits varInits = do
    env <- ask
    mfix $ inEnv $
        (<> env) . M.fromList <$> traverse (\(var, init') -> (var,) <$> eval init') varInits
