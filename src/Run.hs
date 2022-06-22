{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Functions to run code in an environment with standard procedures available.
module Run
    ( runSrcFiles
    , interpS
    , runSrcText
    ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Foldable
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import AstParse
import Conv
import Eval
import Parse
import Paths_ash
import Procedures
import Types
import Util

-- | Loads the standard procedures, then runs the given source files in order. The definitions in
-- each file are visible in the files that follow.
runSrcFiles :: [FilePath] -> IO ()
runSrcFiles files = getDataFileName libFileName >>= runSrcFiles' . (:files)

-- | Loads the standard procedures, then runs the given action.
interpS :: InterpS a -> IO a
interpS action = flip runInterpS nativeProcedures' $
    (liftIO (getDataFileName libFileName >>= parseFile) >>= runTopLevel) *> action

-- | Parses source text as a 'TopLevel' and runs it.
runSrcText
    :: String        -- ^ The file name
    -> T.Text        -- ^ The source text
    -> InterpS SVal
runSrcText name = parseSrcText name >=> runTopLevel

-- | All native standard procedures.
nativeProcedures' :: Env
nativeProcedures' = nativeProcedures <> M.singleton "eval" (Procedure $ MkProcedure eval')

-- | File containing Scheme implementations of remaining standard procedures.
libFileName :: FilePath
libFileName = "lib-scm/lib.scm"

runSrcFiles' :: [FilePath] -> IO ()
runSrcFiles' =
    traverse parseFile >=>
    flip runInterpS nativeProcedures' . traverse_ runTopLevel

parseFile :: FilePath -> IO TopLevel
parseFile file = TIO.readFile file >>= parseSrcText file

parseSrcText :: MonadThrow m => String -> T.Text -> m TopLevel
parseSrcText name =
    either (failParse . T.pack) pure . readManyPrettyErr name >=>
    either (failParse . parseFailMsg) pure . pTopLevel

eval' :: [SVal] -> IO SVal
eval' = convM cList2 . mkSList >=> \(expr, spec) -> do
    expr' <- either (failParse . parseFailMsg) pure $ pExpr expr
    convM cEnvSpec spec >>= \case
        ReportEnv5 -> interpS (runTopLevel (TopLevel [] [expr']))
        NullEnv5   -> runInterpS (runTopLevel (TopLevel [] [expr'])) M.empty
  where
    convM = either (throwM . SException . convFailMsg) pure .: runConv

failParse :: MonadThrow m => T.Text -> m a
failParse = throwM . SException . ("Parse error: " <>)
