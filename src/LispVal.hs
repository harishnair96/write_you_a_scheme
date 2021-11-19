module LispVal where

import Control.Monad.Reader ( ReaderT )
import Data.List (intercalate, intersperse)
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

instance Show LispVal where
  show val = case val of
    Atom atom -> show atom
    String str -> "\\" ++ show str ++ "\\" -- TODO: Avoid using ++
    Number num -> show num
    Bool bool -> if bool then "#t" else "#f"
    Nil -> "Nil"
    List lst -> "(" ++ intercalate "," (fmap show lst) ++ ")" -- TODO: Avoid using ++
    Fun _ -> "(internal function)"
    Lambda _ _ -> "(lambda function)"