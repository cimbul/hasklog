module Main (
  main,
) where

import qualified Prolog.InterpreterSpec
import qualified Prolog.ParserSpec

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Prolog.Interpreter" Prolog.InterpreterSpec.spec
  describe "Prolog.Parser" Prolog.ParserSpec.spec
