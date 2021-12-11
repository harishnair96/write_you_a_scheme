module Repl where

import Control.Exception (throw)
import Control.Monad.Reader
  ( MonadIO (liftIO),
    MonadReader (local),
    ReaderT (runReaderT),
  )
import Data.Either (either)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Eval (eval, evalWithEnv, runEval)
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
      op <- either throwParseError evalParseResult (parseInputs [stdlib, input])
      outputStrLn $ show op
      repl
  where
    throwParseError err = throw $ LispException (T.pack $ show err)
    evalParseResult res = liftIO $ runEval res primEnv

replLoop :: IO ()
replLoop = runInputT defaultSettings repl
