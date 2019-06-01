module Hasklog.InterpreterSpec (
  spec
) where

import Hasklog.Data (Term, Identifier)
import Hasklog.Interpreter
import Hasklog.Parser (parseTest, term)

import qualified Data.Map as M
import Test.Hspec

spec :: Spec
spec =
  describe "unify" $ do
    it "unifies equal constants" $
      shouldUnify "a" "a" []
    it "does not unify unequal constants" $
      shouldNotUnify "a" "b"
    it "does not unify unequal functors" $
      shouldNotUnify "f(a)" "g(a)"
    it "does not unify structures with unequal arities" $
      shouldNotUnify "f(a, b)" "f(a)"
    it "unifies a variable with itself" $
      shouldUnify "X" "X" []
    it "unifies a variable with another variable" $
      shouldUnifyOneWay "X" "Y" [("X", "Y")]
    it "unifies a variable with an atom" $
      shouldUnify "X" "a" [("X", "a")]
    it "unifies a variable with a structure" $
      shouldUnify "X" "f(a, Y)" [("X", "f(a, Y)")]
    it "does not unify a variable with a structure that includes that variable" $
      shouldNotUnify "X" "f(a, X)"
    it "unifies a variable inside a structure to another variable" $
      shouldUnifyOneWay "f(X)" "f(Y)" [("X", "Y")]
    it "unifies a variable inside a structure to an atom" $
      shouldUnify "f(a, X)" "f(a, b)" [("X", "b")] 
    it "unifies a variable inside a structure to another structure" $
      shouldUnify "f(X)" "f(g(Y))" [("X", "g(Y)")]
    it "unifies the same variable with the same constant" $
      shouldUnify "f(X, X)" "f(a, a)" [("X", "a")]
    it "does not unify the same variable to unequal constants" $
      shouldNotUnify "f(X, X)" "f(a, b)"
    it "unifies complex structures with multiple variables" $
      shouldUnify "f(g(X), X)" "f(Y, a)" [("X", "a"), ("Y", "g(a)")]
    it "unifies complex structures with the same variable appearing in both" $
      shouldUnify "f(X, Y)" "f(a, X)" [("X", "a"), ("Y", "a")]

shouldNotUnify :: String -> String -> Expectation
shouldNotUnify a b = do
  unify (parseTerm a) (parseTerm b) `shouldBe` Nothing
  unify (parseTerm b) (parseTerm a) `shouldBe` Nothing

shouldUnify :: String -> String -> [(Identifier, String)] -> Expectation
shouldUnify a b results = do
  shouldUnifyOneWay a b results
  shouldUnifyOneWay b a results

shouldUnifyOneWay :: String -> String -> [(Identifier, String)] -> Expectation
shouldUnifyOneWay a b results = unify (parseTerm a) (parseTerm b) `shouldBe` Just parsedResults
  where
    parsedResults = M.fromList $ map (\(var, val) -> (var, parseTerm val)) results

parseTerm :: String -> Term
parseTerm input =
  case parseTest term input of
    Left _     -> error ("Could not parse " ++ show input)
    Right rslt -> rslt
