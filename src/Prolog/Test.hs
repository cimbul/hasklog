-----------------------------------------------------------------------------
--
-- Module      :  Prolog.Test
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Prolog.Test (

) where

import Prolog.Data
import Prolog.Parser

import Test.HUnit


assertParseError parser input =
  case parseTest parser input of
    Left _       -> return ()
    Right actual -> assertFailure (msg actual)
  where
    msg actual = input ++ "\n" ++
               "expected parse error\n but got: " ++ show actual

assertParse parser input expected =
  case parseTest parser input of
    Left error   -> assertFailure (parseErrorMsg error)
    Right actual -> assertEqual input expected actual
  where
    parseErrorMsg error = input ++ "\n" ++ "expected: " ++ show expected ++
                          "\n but got parse error:\n " ++ show error


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

test_parseTerm = TestList $ map (\(input, expected) ->
    TestCase $ assertParse term input expected
  ) parseTermResults


parseTermFailures = [
  "a b",
  "123abc",
  "@abc",
  ":- a :- b"
 ]

test_parseTermFailure = TestList $ map (\input ->
    TestCase $ assertParseError term input
  ) parseTermFailures


test_parser = TestList [
  test_parseTerm,
  test_parseTermFailure
 ]
