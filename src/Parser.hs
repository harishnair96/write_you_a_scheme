module Parser where

import Control.Monad (MonadPlus (mzero))
import Data.Functor.Identity (Identity)
import qualified Data.Text as T
import LispVal (LispVal (Atom, Bool, Nil, Number, String))
import Text.Parsec (alphaNum, char, digit, letter, many, noneOf, oneOf, sepBy, skipMany1, space, spaces, string, (<|>))
import qualified Text.Parsec.Language as Lang
import Text.Parsec.Text (Parser)
import qualified Text.Parsec.Token as Tok

lexer :: Tok.GenTokenParser T.Text () Identity
lexer =
  Tok.makeTokenParser
    Lang.emptyDef
      { Tok.commentStart = "{-",
        Tok.commentEnd = "-}",
        Tok.commentLine = "--",
        Tok.identStart = letter <|> oneOf "-+/*=|&><", -- identifiers start
        Tok.identLetter = alphaNum <|> oneOf "?+=|&-/",
        Tok.opStart = mzero,
        Tok.opLetter = mzero
      }

parseAtom :: Parser LispVal
parseAtom = do
  tok <- Tok.identifier lexer
  return (Atom $ T.pack tok)

parseString :: Parser LispVal
parseString = do
  char '\"' -- TODO: Check if reservedOp is needed. Check also in other parsers
  str <- many $ noneOf "\""
  char '\"'
  return (String $ T.pack str)

parseNumber :: Parser LispVal -- TODO: Implement parser for decimal numbers
parseNumber = do
  num <- negNum <|> posNum
  return (Number $ read num)
  where
    negNum = do
      char '-'
      num <- many digit
      return ("-" ++ num)
    posNum = many digit

parseNil :: Parser LispVal
parseNil = do
  string "Nil"
  return Nil

parseBool :: Parser LispVal
parseBool = do
  bool <- parseTrue <|> parseFalse
  return (Bool bool)
  where
    parseTrue = do
      string "#t"
      return True
    parseFalse = do
      string "#f"
      return False
