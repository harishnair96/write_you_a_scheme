module Repl where

import Control.Exception (throw)
import Control.Monad.Reader
  ( MonadIO (liftIO),
    MonadReader (local),
    ReaderT (runReaderT),
  )
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Eval (eval, evalSrc, runEval)
import LispVal (EnvCtx, Eval (unEval), LispException (LispException), LispVal)
import Parser (parseContent)
import Prim (primEnv)
import System.Console.Haskeline
  ( InputT,
    defaultSettings,
    getInputLine,
    outputStrLn,
    runInputT,
  )
import Text.Parsec (parse)

type Repl a = InputT IO a

repl :: Repl ()
repl = do
  minput <- getInputLine ">>> "
  case minput of
    Nothing -> outputStrLn "Quitting."
    Just input -> do
      res <- liftIO $ runEval input primEnv
      outputStrLn $ show res
      repl

replLoop :: IO ()
replLoop = runInputT defaultSettings repl
