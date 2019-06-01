module Hasklog.ParserSpec (
  spec
) where

import Hasklog.Data hiding (describe)
import Hasklog.Parser

import Data.Functor.Identity
import Test.Hspec
import Text.Parsec (ParseError)


spec :: Spec
spec =
  describe "term" $ do
    context "with invalid input" $
      mapM_ (itShouldFailOn term) [
        "a b",
        "123abc",
        "@abc",
        ":- a :- b"
       ]
    context "with basic terms" $
      mapM_ (itShouldParse term) [
        ("abc",        Atom "abc"),
        ("abc_123",    Atom "abc_123"),
        ("'Foo bar!'", Atom "Foo bar!"),
        (":-",         Atom ":-"),
        ("@-/~",       Atom "@-/~"),
        ("Xyz",        Variable "Xyz"),
        ("X_22z",      Variable "X_22z"),
        ("123",        Number 123)
      ]
    context "with whitespace" $
      mapM_ (itShouldParse term) [
        ("    abc    ",         Atom "abc"),
        ("% A comment \n  abc", Atom "abc")
      ]
    context "with compound terms" $
      mapM_ (itShouldParse term) [
        ("f(a)",             CompoundTerm "f" [Atom "a"]),
        ("f(a, b)",          CompoundTerm "f" [Atom "a", Atom "b"]),
        ("f(g(a), h(b, c))", CompoundTerm "f" [CompoundTerm "g" [Atom "a"],
                                              CompoundTerm "h" [Atom "b", Atom "c"]])
      ]
    context "with lists" $
      mapM_ (itShouldParse term) [
        ("[]",         Atom "[]"),
        ("[a]",        CompoundTerm "." [Atom "a", Atom "[]"]),
        ("[a, b]",     CompoundTerm "." [Atom "a", CompoundTerm "." [Atom "b", Atom "[]"]]),
        ("[a, b | c]", CompoundTerm "." [Atom "a", CompoundTerm "." [Atom "b", Atom "c"]]),
        ("[[a], [b]]", CompoundTerm "." [CompoundTerm "." [Atom "a", Atom "[]"],
                                         CompoundTerm "." [CompoundTerm "." [Atom "b", Atom "[]"],
                                                           Atom "[]"]])
      ]
    context "with operators" $
      mapM_ (itShouldParse term) [
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


itShouldParse :: (Eq a, Show a) => PrologParser Identity a -> (String, a) -> SpecWith (Arg Expectation)
itShouldParse parser (input, expected) = it ("parses " ++ show input) $
  parseTest parser input `shouldParse` expected

itShouldFailOn :: (Show a) => PrologParser Identity a -> String -> SpecWith (Arg Expectation)
itShouldFailOn parser input = it ("fails to parse " ++ show input) $
  parseTest parser `shouldFailOn` input

shouldFailOn :: Show a => (String -> Either ParseError a) -> String -> Expectation
shouldFailOn parse input =
  case parse input of
    Left _       -> return ()
    Right actual -> expectationFailure ("expected parse error\n but got: " ++ show actual)

shouldParse :: (Eq a, Show a) => Either ParseError a -> a -> Expectation
shouldParse actual expected =
  case actual of
    Left error   -> expectationFailure ("expected: " ++ show expected ++ "\n but got parse error:\n " ++ show error)
    Right actual -> actual `shouldBe` expected
