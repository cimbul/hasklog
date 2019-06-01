module Main (
  main,
) where

import qualified Hasklog.InterpreterSpec
import qualified Hasklog.ParserSpec

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Hasklog.Interpreter" Hasklog.InterpreterSpec.spec
  describe "Hasklog.Parser" Hasklog.ParserSpec.spec
