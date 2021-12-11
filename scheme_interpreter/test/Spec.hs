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
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
  ( Expectation,
    Spec,
    describe,
    hspec,
    it,
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

parsed :: String -> (LispVal -> Expectation) -> Expectation
parsed input assert =
  let parseResult = parseInput input
   in case parseResult of
        Left lv -> fail (show lv)
        Right res -> assert res
