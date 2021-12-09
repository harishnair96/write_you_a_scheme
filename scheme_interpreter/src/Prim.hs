{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Prim (primEnv) where

import Control.Exception (throw)
import Control.Monad (foldM)
import Control.Monad.Reader (MonadIO (liftIO))
import Data.Map.Strict (fromList)
import qualified Data.Text as T
import LispVal (EnvCtx, Eval, IFunc (IFunc), LispException (LispException), LispVal (Atom, Bool, Fun, List, Nil, Number, String))
import System.Directory (doesFileExist)

primEnv :: EnvCtx -- Holds all primitive functions
primEnv =
  fromList
    [ ("+", addOp),
      ("*", mulOp),
      ("++", strConcatOp),
      ("-", subOp),
      ("<", ltOp),
      (">", gtOp),
      (">=", gteOp),
      ("<=", lteOp),
      ("==", eqOp),
      ("even?", evenOp),
      ("odd?", oddOp),
      ("pos?", posOp),
      ("neg?", negOp),
      ("eq?", lispEqOp),
      ("bl-eq?", blEqOp),
      ("and", andOp),
      ("or", orOp),
      ("cons", consOp),
      ("car", carOp),
      ("cdr", cdrOp),
      ("file?", fileExistsOp),
      ("slurp", slurpOp),
      ("nil?", nilOp)
    ]

mkFn :: ([LispVal] -> Eval LispVal) -> LispVal
mkFn = Fun . IFunc

type Binary = LispVal -> LispVal -> Eval LispVal

type Unary = LispVal -> Eval LispVal

addOp :: LispVal
addOp = mkFn $ binopFold' (numOp (+))

mulOp :: LispVal
mulOp = mkFn $ binopFold' (numOp (*))

strConcatOp :: LispVal
strConcatOp = mkFn $ binopFold' (strOp T.append)

subOp :: LispVal
subOp = mkFn $ \case
  [] -> throw $ LispException "No args provided for subOp"
  [arg] -> numOp (*) (Number (-1)) arg
  args -> binopFold' (numOp (-)) args

ltOp :: LispVal
ltOp = mkFn $ binOp (numCmp (<))

gtOp :: LispVal
gtOp = mkFn $ binOp (numCmp (>))

lteOp :: LispVal
lteOp = mkFn $ binOp (numCmp (<=))

gteOp :: LispVal
gteOp = mkFn $ binOp (numCmp (>=))

eqOp :: LispVal -- Compare if 2 numbers are equal
eqOp = mkFn $ binOp (numCmp (==))

blEqOp :: LispVal -- Compare if 2 bools are equal
blEqOp = mkFn $ binOp (boolOp (==))

lispEqOp :: LispVal -- Compare if 2 lisp vals are equal
lispEqOp = mkFn $
  binOp $ \x y -> case (x, y) of
    (Number _, Number _) -> numCmp (==) x y
    (Nil, Nil) -> return $ Bool True
    (Bool _, Bool _) -> boolOp (==) x y
    (Atom a1, Atom a2) -> return $ Bool $ a1 == a2
    (String s1, String s2) -> return $ Bool $ s1 == s2
    _ -> return $ Bool False

evenOp :: LispVal
evenOp = mkFn $ unOp (numBool even)

oddOp :: LispVal
oddOp = mkFn $ unOp (numBool odd)

posOp :: LispVal
posOp = mkFn $ unOp (numBool (> 0))

negOp :: LispVal
negOp = mkFn $ unOp (numBool (< 0))

andOp :: LispVal
andOp = mkFn $ binopFold' (boolOp (&&))

orOp :: LispVal
orOp = mkFn $ binopFold' (boolOp (||))

consOp :: LispVal
consOp = mkFn $
  binOp $ \x y -> case (x, y) of
    (val, List l) -> return $ List $ val : l
    _ -> throw $ LispException "Invalid operands to cons"

carOp :: LispVal
carOp = mkFn $
  unOp $ \case
    List [] -> return Nil
    List (x : xs) -> return x
    _ -> throw $ LispException "Invalid operands to car"

cdrOp :: LispVal
cdrOp = mkFn $
  unOp $ \case
    List [] -> return Nil
    List (x : xs) -> return $ List xs
    _ -> throw $ LispException "Invalid operands to car"

fileExistsOp :: LispVal
fileExistsOp = mkFn $
  unOp $ \case
    Atom fName -> fileExists' fName
    String fName -> fileExists' fName
    _ -> throw $ LispException "Invalid input to fileExists"
  where
    fileExists' fName = do
      exists <- liftIO $ doesFileExist $ T.unpack fName
      return $ Bool exists

slurpOp :: LispVal
slurpOp = mkFn $
  unOp $ \case
    Atom fName -> readFile' fName
    String fName -> readFile' fName
    _ -> throw $ LispException "Invalid input to slurp"
  where
    readFile' fName = do
      contents <- liftIO $ readFile $ T.unpack fName
      return $ String $ T.pack contents

nilOp :: LispVal
nilOp = mkFn $
  unOp $ \case
    List l -> return $ Bool $ null l
    _ -> throw $ LispException "Invalid argument to nil?"

-- The following helper functions take a haskell function and convert into lisp function
numOp :: (Integer -> Integer -> Integer) -> Binary
numOp op arg1 arg2 = case (arg1, arg2) of
  (Number n1, Number n2) -> return $ Number (op n1 n2)
  _ -> throw $ LispException "Invalid operands to num op"

boolOp :: (Bool -> Bool -> Bool) -> Binary
boolOp op arg1 arg2 = case (arg1, arg2) of
  (Bool b1, Bool b2) -> return $ Bool (op b1 b2)
  _ -> throw $ LispException "Inavlid operands for bool"

numCmp :: (Integer -> Integer -> Bool) -> Binary
numCmp op arg1 arg2 = case (arg1, arg2) of
  (Number n1, Number n2) -> return $ Bool (op n1 n2)
  _ -> throw $ LispException "Invalid operands to numCmp op"

numBool :: (Integer -> Bool) -> Unary
numBool op arg = case arg of
  Number n -> return (Bool $ op n)
  _ -> throw $ LispException "Invalid operands to numbool"

strOp :: (T.Text -> T.Text -> T.Text) -> Binary
strOp op arg1 arg2 = case (arg1, arg2) of
  (String str1, String str2) -> return (String $ op str1 str2)
  _ -> throw $ LispException "Invalid operands to str op"

-- Fold using binary function
binopFold :: Binary -> LispVal -> [LispVal] -> Eval LispVal
binopFold op arg1 args = case args of
  [] -> throw $ LispException "No args given"
  _ -> foldM op arg1 args

binopFold' :: Binary -> [LispVal] -> Eval LispVal
binopFold' op args = case args of
  [] -> throw $ LispException "No args given"
  (arg : args) -> binopFold op arg args

-- Binary operation
binOp :: Binary -> [LispVal] -> Eval LispVal
binOp op args = case args of
  [] -> throw $ LispException "Not enough operands for binary operation"
  [arg1, arg2] -> op arg1 arg2
  _ -> throw $ LispException "Too many args for binary operation"

unOp :: Unary -> [LispVal] -> Eval LispVal
unOp op args = case args of
  [] -> throw $ LispException "Not enough operands for unary operation"
  [arg] -> op arg
  _ -> throw $ LispException "Too many operands for unary operation"
