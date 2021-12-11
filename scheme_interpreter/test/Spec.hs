{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Data.Text as T
import Eval (eval)
import LispVal
  ( Eval (unEval),
    LispVal (Atom, Bool, List, Nil, Number, String),
  )
import Parser (parseInput)
import Prim (primEnv)
import Repl (runLisp)
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
  ( Expectation,
    Spec,
    describe,
    hspec,
    it,
    runIO,
    shouldBe,
  )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parser" $ do
    it "Atom" $
      parsed "atom1" (`shouldBe` Atom "atom1")

    it "Negative Number" $
      parsed "-5" (`shouldBe` (Number $ -5))

    it "Positive Number" $
      parsed "5" (`shouldBe` Number 5)

    it "String" $
      parsed "\"string\"" (`shouldBe` String "string")

    it "String" $
      parsed "Nil" (`shouldBe` Nil)

    it "True" $
      parsed "#t" (`shouldBe` Bool True)

    it "False" $
      parsed "#f" (`shouldBe` Bool False)

    it "List" $
      parsed "5 Nil atom1 #t" (`shouldBe` List [Number 5, Nil, Atom "atom1", Bool True])

    it "S-Expr" $
      parsed "(5 Nil atom1 #t)" (`shouldBe` List [Number 5, Nil, Atom "atom1", Bool True])

    it "` Quoted" $
      parsed "`(5 Nil)" (`shouldBe` List [Atom "quote", List [Number 5, Nil]])

    it "' Quoted" $
      parsed "'(5 Nil)" (`shouldBe` List [Atom "quote", List [Number 5, Nil]])

  describe "StdLib" $ do
    it "id" $
      evaled "(id 5)" (`shouldBe` Number 5)

parsed :: String -> (LispVal -> Expectation) -> Expectation
parsed input assert = either (fail . show) assert (parseInput input)

evaled :: String -> (LispVal -> Expectation) -> Expectation
evaled input assert = do
  let stdLib = unsafePerformIO $ readFile "./src/stdlib.scm" -- TODO: Check if unsafe is ok
      res = unsafePerformIO $ runLisp [stdLib, input] primEnv
  assert res