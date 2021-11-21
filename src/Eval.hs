{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Reader
import Data.Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import LispVal (EnvCtx, Eval, LispVal (Atom, Bool, List, Nil, Number, String))

-- TODO: Use pattern synonyms?
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
  List [Atom "let", List bindings, body] -> lispLet bindings body

getFromEnv :: T.Text -> Eval LispVal
getFromEnv key = do
  envCtx <- ask
  let res = Map.lookup key envCtx
  case res of
    Just val -> return val
    Nothing -> error $ "Unable to find name" ++ T.unpack key -- TODO: Proper error handling. Make function total.

lispIf :: LispVal -> LispVal -> LispVal -> Eval LispVal
lispIf cond onTrue onFalse = do
  bool <- eval cond
  case bool of
    Bool True -> return onTrue
    Bool False -> return onFalse
    _ -> error "Provided non boolean value for if condition" -- TODO: Proper error handling. Make function total.

lispLet :: [LispVal] -> LispVal -> Eval LispVal
lispLet bindings body = do
  envCtx <- ask
  let insertEnv env (List [Atom atom, val]) = do
        val' <- eval val
        return $ Map.insert atom val env
      insertEnv _ _ = error "Invalid form on let statement" -- TODO: Proper error handling. Make function total.
  newEnv <- foldM insertEnv envCtx bindings
  local (const newEnv) (eval body)