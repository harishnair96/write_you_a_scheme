module Repl where

import Control.Exception (SomeException, throw)
import Control.Monad.Reader
  ( MonadIO (liftIO),
    MonadReader (local),
    ReaderT (runReaderT),
  )
import Data.Either (either)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Eval (runEval)
import LispVal (EnvCtx, Eval (unEval), LispException (LispException), LispVal)
import Parser (parseInput, parseInputs)
import Prim (primEnv)
import System.Console.Haskeline
  ( InputT,
    defaultSettings,
    getInputLine,
    outputStrLn,
    runInputT,
  )

type Repl a = InputT IO a

repl :: Repl ()
repl = do
  minput <- getInputLine ">>> "
  stdlib <- liftIO $ readFile "./src/stdlib.scm"
  case minput of
    Nothing -> outputStrLn "Quitting."
    Just input -> do
      op <- liftIO $ runLisp [stdlib, input] primEnv
      outputStrLn $ either show show op
      repl
  where
    throwParseError err = throw $ LispException (T.pack $ show err)
    evalParseResult res = liftIO $ runEval res primEnv

replLoop :: IO ()
replLoop = runInputT defaultSettings repl

runLisp :: [String] -> EnvCtx -> IO (Either SomeException LispVal)
runLisp inputs env = either throwParseError evalParseResult (parseInputs inputs)
  where
    throwParseError err = throw $ LispException (T.pack $ show err)
    evalParseResult res = runEval res env
