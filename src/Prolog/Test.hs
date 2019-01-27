module Main (
  main,
) where

import Prolog.Data
import Prolog.Parser
import Prolog.Interpreter

import Data.Functor.Identity
import qualified Data.Map as M

import Test.HUnit


p :: String -> Term
p input =
  case parseTest term input of
    Left _     -> error ("Could not parse \"" ++ input ++ "\"")
    Right rslt -> rslt


-- Parser


assertParseError :: Show a => PrologParser Identity a -> String -> IO ()
assertParseError parser input =
  case parseTest parser input of
    Left _       -> return ()
    Right actual -> assertFailure (msg actual)
  where
    msg actual = input ++ "\n" ++
               "expected parse error\n but got: " ++ show actual

assertParse :: (Eq a, Show a) => PrologParser Identity a -> String -> a -> IO ()
assertParse parser input expected =
  case parseTest parser input of
    Left error   -> assertFailure (parseErrorMsg error)
    Right actual -> assertEqual input expected actual
  where
    parseErrorMsg error = input ++ "\n" ++ "expected: " ++ show expected ++
                          "\n but got parse error:\n " ++ show error


parseTermResults :: [(String, Term)]
parseTermResults = [
  -- Basic terms
  ("abc",        Atom "abc"),
  ("abc_123",    Atom "abc_123"),
  ("'Foo bar!'", Atom "Foo bar!"),
  (":-",         Atom ":-"),
  ("@-/~",       Atom "@-/~"),
  ("Xyz",        Variable "Xyz"),
  ("X_22z",      Variable "X_22z"),
  ("123",        Number 123),

  -- Whitespace
  ("    abc    ",         Atom "abc"),
  ("% A comment \n  abc", Atom "abc"),

  -- Compound terms
  ("f(a)",             CompoundTerm "f" [Atom "a"]),
  ("f(a, b)",          CompoundTerm "f" [Atom "a", Atom "b"]),
  ("f(g(a), h(b, c))", CompoundTerm "f" [CompoundTerm "g" [Atom "a"],
                                         CompoundTerm "h" [Atom "b", Atom "c"]]),

  -- Lists
  ("[]",         Atom "[]"),
  ("[a]",        CompoundTerm "." [Atom "a", Atom "[]"]),
  ("[a, b]",     CompoundTerm "." [Atom "a", CompoundTerm "." [Atom "b", Atom "[]"]]),
  ("[a, b | c]", CompoundTerm "." [Atom "a", CompoundTerm "." [Atom "b", Atom "c"]]),
  ("[[a], [b]]", CompoundTerm "." [CompoundTerm "." [Atom "a", Atom "[]"],
                                   CompoundTerm "." [CompoundTerm "." [Atom "b", Atom "[]"],
                                                     Atom "[]"]]),

  -- Operators
  ("a :- b",       CompoundTerm ":-" [Atom "a", Atom "b"]),
  ("a :- b, c",    CompoundTerm ":-" [Atom "a", CompoundTerm "," [Atom "b", Atom "c"]]),
  ("a :- b, c, d", CompoundTerm ":-" [Atom "a",
                                      CompoundTerm "," [Atom "b",
                                                        CompoundTerm "," [Atom "c",
                                                                          Atom "d"]]]),
  ("not a, b",     CompoundTerm "," [CompoundTerm "not" [Atom "a"], Atom "b"]),
  ("f(a, b)",      CompoundTerm "f" [Atom "a", Atom "b"]),
  ("f((a, b))",    CompoundTerm "f" [CompoundTerm "," [Atom "a", Atom "b"]]),
  ("(a :- b), c",  CompoundTerm "," [CompoundTerm ":-" [Atom "a", Atom "b"], Atom "c"])
 ]

test_parseTerm :: Test
test_parseTerm = TestList $ map (\(input, expected) ->
    TestCase $ assertParse term input expected
  ) parseTermResults


parseTermFailures :: [String]
parseTermFailures = [
  "a b",
  "123abc",
  "@abc",
  ":- a :- b"
 ]

test_parseTermFailure :: Test
test_parseTermFailure = TestList $ map (TestCase . assertParseError term) parseTermFailures


test_parser :: Test
test_parser = TestList [
  test_parseTerm,
  test_parseTermFailure
 ]




-- Unification

unifySuccesses :: [(String, String, [(String, String)])]
unifySuccesses = [
  ("a", "a", []),
  ("X", "a", [("X", "a")]),
  ("a", "X", [("X", "a")]),
  ("X", "X", []),
  ("X", "Y", [("X", "Y")]),

  ("X",          "f(a, b)", [("X", "f(a, b)")]),
  ("f(a, X)",    "f(a, b)", [("X", "b")]),
  ("f(X)",       "f(Y)",    [("X", "Y")]),
  ("f(X)",       "f(g(Y))", [("X", "g(Y)")]),
  ("f(g(X), X)", "f(Y, a)", [("X", "a"), ("Y", "g(a)")]),
  ("f(X, Y)",    "f(a, X)", [("X", "a"), ("Y", "a")]),
  ("f(X, X)",    "f(a, a)", [("X", "a")])
 ]

test_unifySuccesses :: Test
test_unifySuccesses = TestList $ map unifySuccessTestCase unifySuccesses

  where

    unifySuccessTestCase (a, b, results) =
        label ~: unify (p a) (p b) ~?= Just (M.fromList parsedResults)
      where
        label = a ++ " == " ++ b
        parsedResults = map (\(var, val) -> (var, p val)) results


unifyFailures :: [(String, String)]
unifyFailures = [
  ("a",       "b"),      -- Unequal constants
  ("f(a)",    "g(a)"),   -- Unequal functors
  ("f(a, b)", "f(a)"),   -- Unequal arities
  ("X",       "f(X)"),   -- Occurs check
  ("f(X, X)", "f(a, b)") -- Double unification to unequal constants
 ]

test_unifyFailures :: Test
test_unifyFailures = TestList $ map unifyFailureTestCase unifyFailures

  where

    unifyFailureTestCase (a, b) =
        label ~: unify (p a) (p b) ~?= Nothing
      where
        label = a ++ " == " ++ b


test_unification :: Test
test_unification = TestList [
   test_unifySuccesses,
   test_unifyFailures
 ]


tests :: Test
tests = TestList [
   test_parser,
   test_unification
 ]

main :: IO Counts
main = runTestTT tests
