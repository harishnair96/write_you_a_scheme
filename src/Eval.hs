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
  List [Atom "let", List bindings, body] -> lispLet bindings body -- accepts a list of bindings and runs body in the modified environment
  List [Atom "define", atom, val] -> lispDefine atom val
  List (Atom "begin" : rest) -> lispBegin rest

-- TODO: Extract common functions like set env
-- TODO: Use the term expression instead of statement/operations
-- TODO: Use error messages from real scheme interpreter

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

lispDefine :: LispVal -> LispVal -> Eval LispVal
lispDefine atom value = do
  envCtx <- ask
  evalVal <- eval value
  case atom of
    Atom atom -> local (const $ Map.insert atom evalVal envCtx) (return value) -- TODO: Check what should be returned here?
    _ -> error "Invalid form of define statement" -- TODO: Proper error handling

lispBegin :: [LispVal] -> Eval LispVal
lispBegin exprs = case exprs of
  [] -> error "No operands to begin" -- TODO: Proper error handling
  [expr] -> eval expr
  (expr : exprs) -> do
    eval expr
    lispBegin exprs