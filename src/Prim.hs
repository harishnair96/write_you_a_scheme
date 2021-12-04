{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (throw)
import Control.Monad (foldM)
import Data.Map.Strict (fromList)
import qualified Data.Text as T
import LispVal (EnvCtx, Eval, IFunc (IFunc), LispException (LispException), LispVal (Fun, Number, String))

primEnv :: EnvCtx -- Holds all primitive functions
primEnv =
  fromList
    [ ("+", lispAdd),
      ("*", lispMul),
      ("++", lispConcat)
    ]

mkFn :: ([LispVal] -> Eval LispVal) -> LispVal
mkFn = Fun . IFunc

type Binary = LispVal -> LispVal -> Eval LispVal

type Unary = LispVal -> Eval LispVal

lispAdd :: LispVal
lispAdd = mkFn $ binopFold (numOp (+)) (Number 0)

lispMul :: LispVal
lispMul = mkFn $ binopFold (numOp (*)) (Number 1)

lispConcat :: LispVal
lispConcat = mkFn $ binopFold (strOp T.append) (String "")

-- Helper function to create binary functions on Number from Haskell function
numOp :: (Integer -> Integer -> Integer) -> LispVal -> LispVal -> Eval LispVal
numOp op arg1 arg2 = case (arg1, arg2) of
  (Number n1, Number n2) -> return $ Number (op n1 n2)
  _ -> throw $ LispException "Invalid operands to num op"

-- Helper function to create binary functions on String from Haskell function
strOp :: (T.Text -> T.Text -> T.Text) -> LispVal -> LispVal -> Eval LispVal
strOp op arg1 arg2 = case (arg1, arg2) of
  (String str1, String str2) -> return (String $ op str1 str2)
  _ -> throw $ LispException "Invalid operands to str op"

-- Fold using binary function
binopFold :: Binary -> LispVal -> [LispVal] -> Eval LispVal
binopFold op arg1 args = case args of
  [] -> throw $ LispException "No args given"
  _ -> foldM op arg1 args