{-# LANGUAGE OverloadedStrings #-}

import LispVal (Eval, LispVal (Atom, Bool, List, Nil, Number, String))
import qualified Data.Text as T

eval :: LispVal -> Eval LispVal
eval lispVal = case lispVal of
  str@(String _) -> return str
  num@(Number _) -> return num
  bool@(Bool _) -> return bool
  Nil -> return Nil
  List [] -> return Nil -- Empty lists are treated as Nil
  List [Atom "quote", val] -> return val -- Return quoted value as such
  List [Atom "write", val] -> return (String $ T.pack $ show val)
  List (Atom "write":rest) -> return (String $ T.pack $ show $ List rest)
  _ -> _