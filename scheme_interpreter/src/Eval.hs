{-# LANGUAGE OverloadedStrings #-}

--{-# LANGUAGE ScopedTypeVariables #-}

module Eval (eval, evalSrc, runEval) where

import Control.Exception (Exception (fromException), SomeException, throw, try)
import Control.Monad.Reader
import Data.Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import LispVal (EnvCtx, Eval (unEval), IFunc (IFunc), LispException (LispException), LispVal (Atom, Bool, Fun, Lambda, List, Nil, Number, String))
import Parser (parseContent)
import Text.Parsec (parse)

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
  List [List [Atom "define", atom, val], rest] -> lispDefine atom val rest
  List (List [Atom "define", atom, val] : rest) -> lispDefine atom val (List rest)
  List (Atom "begin" : rest) -> lispBegin rest
  List [Atom "lambda", List params, body] -> lispLambda params body
  List [Atom "cdr", List [Atom "quote", List (x : xs)]] -> return $ List xs
  List [Atom "car", List [Atom "quote", List (x : xs)]] -> return x
  List (x : xs) -> applyFunc x xs
  _ -> throw (LispException "error")

-- TODO: Extract common functions like set env
-- TODO: Use the term expression instead of statement/operations
-- TODO: Use error messages from real scheme interpreter

-- Evaluates the provided input with primitive environment
evalSrc :: String -> EnvCtx -> Eval LispVal
evalSrc src env = do
  let parseResult = parse parseContent "" (T.pack src)
  case parseResult of
    Left pe -> throw $ LispException (T.pack $ show pe)
    Right lv -> local (const env) (eval lv)

runEval :: String -> EnvCtx -> IO LispVal
runEval src env = do
  runReaderT (unEval $ evalSrc src env) emptyEnv

emptyEnv :: EnvCtx
emptyEnv = Map.empty

getFromEnv :: T.Text -> Eval LispVal
getFromEnv key = do
  envCtx <- ask
  let res = Map.lookup key envCtx
  case res of
    Just val -> return val
    Nothing -> throw $ LispException $ T.concat ["Unable to find name ", key]

lispIf :: LispVal -> LispVal -> LispVal -> Eval LispVal
lispIf cond onTrue onFalse = do
  bool <- eval cond
  case bool of
    Bool True -> return onTrue
    Bool False -> return onFalse
    _ -> throw $ LispException "Provided non boolean value for if condition"

lispLet :: [LispVal] -> LispVal -> Eval LispVal
lispLet bindings body = do
  envCtx <- ask
  let insertEnv env (List [Atom atom, val]) = do
        val' <- eval val
        return $ Map.insert atom val env
      insertEnv _ _ = throw $ LispException "Invalid form on let statement"
  newEnv <- foldM insertEnv envCtx bindings
  local (const newEnv) (eval body)

lispDefine :: LispVal -> LispVal -> LispVal -> Eval LispVal
lispDefine atom value rest = do
  envCtx <- ask
  evalValue <- eval value
  case atom of
    Atom atom -> local (const $ Map.insert atom evalValue envCtx) (eval rest)
    _ -> throw $ LispException "Invalid form of define statement"

lispBegin :: [LispVal] -> Eval LispVal
lispBegin exprs = case exprs of
  [] -> throw $ LispException "No operands to begin"
  [expr] -> eval expr
  (expr : exprs) -> do
    eval expr
    lispBegin exprs

lispLambda :: [LispVal] -> LispVal -> Eval LispVal
lispLambda params body = do
  envCtx <- ask
  let func = IFunc $ \args -> do
        evalArgs <- mapM eval args
        let insertMap env (Atom param, val) = Map.insert param val env
            insertMap _ _ = throw $ LispException "Invalid form of lambda"
        let envCtx' = foldl' insertMap envCtx (zip params evalArgs)
        local (const envCtx') (eval body)
  return (Lambda func envCtx)

applyFunc :: LispVal -> [LispVal] -> Eval LispVal
applyFunc op args = do
  evalArgs <- mapM eval args
  func <- eval op
  case func of
    Fun (IFunc fun) -> fun evalArgs
    Lambda (IFunc fun) envCtx -> local (const envCtx) (fun evalArgs)
    _ -> throw $ LispException "Invalid function"
