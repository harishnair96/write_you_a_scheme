{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module LispVal where

import Control.Exception (Exception)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)
import Data.List (intercalate, intersperse)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

data LispVal -- TODO: Use GADT?
  = Atom T.Text -- Similar to variables
  | String T.Text
  | Number Integer -- TODO: Check how to use Num
  | List [LispVal]
  | Fun IFunc -- TODO: Should this be a LispVal as this is also just a List?
  | Lambda IFunc EnvCtx -- TODO: Should this be a LispVal as this is also just a List?
  | Nil
  | Bool Bool

newtype IFunc = IFunc {fn :: [LispVal] -> Eval LispVal}

-- TODO: Check why newtype is used instead of type as using type could possible avoid GeneralisedNewtypeDeriving extension?
newtype Eval a = Eval {unEval :: ReaderT EnvCtx IO a}
  deriving newtype
    ( Monad,
      Applicative, -- Necessary in order to derive Monad
      Functor, -- Necessary in order to derive Applicative
      MonadReader EnvCtx, -- For ask and local function
      MonadIO -- For liftIO
    )

type EnvCtx = Map.Map T.Text LispVal

instance Eq LispVal where
  (==) v1 v2 = case (v1, v2) of
    (Atom a1, Atom a2) -> a1 == a2
    (String s1, String s2) -> s1 == s2
    (Number n1, Number n2) -> n1 == n2
    (List vs1, List vs2) -> vs1 == vs2
    (Nil, Nil) -> True
    (Bool b1, Bool b2) -> b1 == b2
    _ -> False

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

data LispException = LispException T.Text deriving (Exception) -- TODO: Add more specific exceptions

instance Show LispException where
  show (LispException msg) = T.unpack msg