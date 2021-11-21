{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Reader
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import LispVal (Eval, LispVal (Atom, Bool, List, Nil, Number, String))

eval :: LispVal -> Eval LispVal
eval lispVal = case lispVal of
  str@(String _) -> return str
  num@(Number _) -> return num
  bool@(Bool _) -> return bool
  Nil -> return Nil
  Atom atom -> getFromEnv atom
  List [] -> return Nil -- Empty lists are treated as Nil
  List [Atom "quote", val] -> return val -- quote operator returns value as such without evaluation
  List [Atom "write", val] -> return (String $ T.pack $ show val) -- write operator returns string representation of value without evaluation
  List (Atom "write" : rest) -> return (String $ T.pack $ show $ List rest)
  List [Atom "if", cond, onTrue, onFalse] -> lispIf cond onTrue onFalse
  _ -> _

getFromEnv :: T.Text -> Eval LispVal
getFromEnv key = do
  envCtx <- ask
  let res = Map.lookup key envCtx
  case res of
    Just val -> return val
    Nothing -> error $ "Unable to find name" ++ T.unpack key -- TODO: Proper error handling

lispIf :: LispVal -> LispVal -> LispVal -> Eval LispVal
lispIf cond onTrue onFalse = do
  bool <- eval cond
  case bool of
    Bool True -> return onTrue
    Bool False -> return onFalse
    _ -> error "Provided non boolean value for if condition" -- TODO: Proper error handling