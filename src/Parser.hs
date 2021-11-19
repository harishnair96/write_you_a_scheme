{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Monad (MonadPlus (mzero))
import Data.Functor.Identity (Identity)
import qualified Data.Text as T
import LispVal (LispVal (Atom, Bool, List, Nil, Number, String))
import Text.Parsec (alphaNum, char, digit, letter, many, many1, newline, noneOf, oneOf, sepBy, skipMany1, space, spaces, string, try, (<|>))
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

parseExpr :: Parser LispVal
parseExpr =
  try parseNil
    <|> try parseBool
    <|> try parseNumber
    <|> try parseAtom
    <|> try parseString
    <|> try parseQuote
    <|> try parseSExpr

parseAtom :: Parser LispVal -- TODO: Atoms can contain more than alphanum (parse other operators too)
parseAtom = do
  tok <- many1 alphaNum
  return (Atom $ T.pack tok)

parseString :: Parser LispVal
parseString = do
  char '\"' -- TODO: Check if reservedOp is needed. Check also in other parsers
  str <- many $ noneOf "\""
  char '\"'
  return (String $ T.pack str)

parseNumber :: Parser LispVal -- TODO: Implement parser for decimal numbers
parseNumber = do
  -- TODO: Fix - parseNumber also parses numbers like 4a
  num <- negNum <|> posNum
  return (Number $ read num)
  where
    negNum = do
      char '-'
      num <- many1 digit
      return ("-" ++ num)
    posNum = many1 digit

parseNil :: Parser LispVal
parseNil = do
  string "Nil"
  return Nil

parseBool :: Parser LispVal
parseBool = do
  bool <- try parseTrue <|> parseFalse
  return (Bool bool)
  where
    parseTrue = do
      string "#t"
      return True
    parseFalse = do
      string "#f"
      return False

parseList :: Parser LispVal
parseList = do
  items <- sepBy parseExpr (many1 space <|> many1 newline)
  return (List items)

parseSExpr :: Parser LispVal
parseSExpr = do
  char '('
  spaces
  items <- parseList
  spaces
  char ')'
  return items

parseQuote :: Parser LispVal -- TODO: Use separate type for quote?
parseQuote = do
  char '\''
  body <- parseExpr
  return (List [Atom "quote", body])
