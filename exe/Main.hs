{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Catch
import Control.Monad.Trans.Class
import Data.Version
import System.Console.Haskeline
import System.Environment
import qualified Data.Text as T

import Paths_ash
import Run
import Types

main :: IO ()
main = getArgs >>= \case
    ["-h"] -> do
        putStrLn "Usage: ash [file...]"
        putStrLn "Launches the REPL when there are no files"
    []     -> repl
    files  -> handleAll (putStrLn . ("Error: " ++) . displayException) $ runSrcFiles files

data ReplResult
    = RStr T.Text
    | RExc T.Text
    | RNothing

repl :: IO ()
repl = interpS $ runInputT defaultSettings (greet *> loop)
  where
    greet = do
        outputStrLn $ "ash v" ++ showVersion version
        outputStrLn ",q to quit"
    loop = getInputLine "> " >>= \case
        Nothing   -> pure ()
        Just ",q" -> pure ()
        Just input -> do
            lift (processLine $ T.pack input) >>= \case
                RStr s -> outputStrLn $ T.unpack s
                RExc e -> outputStrLn $ T.unpack $ "Error: " <> e
                _      -> pure ()
            loop

processLine :: T.Text -> InterpS ReplResult
processLine inp =
    handleAll (pure . RExc . T.pack . displayException) (toReplResult <$> runSrcText "<repl>" inp)
  where
    toReplResult = \case
        Unspec -> RNothing
        x      -> RStr $ writeVal x
