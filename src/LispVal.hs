module LispVal where

import Control.Monad.Reader
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

data LispVal
  = Atom T.Text
  | String T.Text
  | Number Integer
  | List [LispVal]
  | Fun IFunc
  | Lambda IFunc EnvCtx
  | Nil
  | Bool Bool 

newtype IFunc = IFunc {fn :: [LispVal] -> Eval LispVal}

newtype Eval a = Eval {unEval :: ReaderT EnvCtx IO a}

type EnvCtx = Map.Map T.Text LispVal