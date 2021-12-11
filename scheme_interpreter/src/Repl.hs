module Repl where

import Control.Monad.Reader (MonadIO (liftIO), ReaderT (runReaderT))
import qualified Data.Text as T
import Eval (eval)
import LispVal (Eval (unEval), LispVal)
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
      let parseResult = parse parseContent "" (T.pack input)
      case parseResult of
        Left pe -> outputStrLn (show pe)
        Right lv -> do
          outputStrLn (show lv)
          res <- liftIO $ runReaderT (unEval $ eval lv) primEnv
          outputStrLn $ show res
          repl

replLoop :: IO ()
replLoop = runInputT defaultSettings repl