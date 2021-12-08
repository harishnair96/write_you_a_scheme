{-# LANGUAGE OverloadedStrings #-}

module Parser (parseContent) where

import Control.Monad (MonadPlus (mzero))
import Data.Functor.Identity (Identity)
import qualified Data.Text as T
import Debug.Trace (traceM)
import LispVal (LispVal (Atom, Bool, List, Nil, Number, String))
import Text.Parsec (alphaNum, char, letter, oneOf, sepBy, try, (<|>))
import qualified Text.Parsec.Language as Lang
import Text.Parsec.Text (Parser)
import Text.Parsec.Token (GenTokenParser (reserved))
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

parseContent :: Parser LispVal
parseContent = do
  Tok.whiteSpace lexer
  parseList

parseExpr :: Parser LispVal
parseExpr = do
  parseNil
    <|> parseAtom
    <|> parseBool
    <|> parseNumber
    <|> parseString
    <|> parseQuote
    <|> parseSExpr

parseAtom :: Parser LispVal
parseAtom = do
  tok <- Tok.identifier lexer
  return (Atom $ T.pack tok)

parseString :: Parser LispVal
parseString = do
  str <- Tok.stringLiteral lexer
  return (String $ T.pack str)

parseNumber :: Parser LispVal -- TODO: Implement parser for floating numbers
parseNumber = try $ do
  num <- Tok.integer lexer
  return (Number num)

parseNil :: Parser LispVal
parseNil = try $ do
  reserved lexer "Nil"
  return Nil

parseBool :: Parser LispVal
parseBool = try $ do
  true <|> false

true :: Parser LispVal
true = do
  reserved lexer "#t"
  return (Bool True)

false :: Parser LispVal
false = do
  reserved lexer "#f"
  return (Bool False)

parseList :: Parser LispVal
parseList = do
  items <- sepBy parseExpr (Tok.whiteSpace lexer)
  case items of
    [x] -> return x
    _ -> return (List items)

parseSExpr :: Parser LispVal
parseSExpr = Tok.parens lexer parseList

parseQuote :: Parser LispVal
parseQuote = do
  char '\''
  body <- parseSExpr
  return (List [Atom "quote", body])
