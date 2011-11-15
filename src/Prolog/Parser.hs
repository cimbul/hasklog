-----------------------------------------------------------------------------
--
-- Module      :  Prolog.Parser
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

module Prolog.Parser (
  parseProgram,
  parseQuery,
) where


import Prolog.Data

import Control.Applicative ((<$>), (<*>), (<*))
import Text.ParserCombinators.Parsec


parseProgram = parse program
parseQuery   = parse query


-- Program/Query parsers

query = GoalClause <$> (goal <* eof)
  where
    goal = reservedSym "?-" *> conjunction <* reservedSym "."

program = Program <$> (clauses <* eof)

clauses = clause `endBy` reservedSym "."

clause = DefiniteClause <$> clauseHead <*> option [] clauseBody
  where
    clauseHead = term
    clauseBody = reservedSym ":-" *> many1 term <?> "body of clause"

conjunction = term `sepBy` reservedSym ","


-- Term parsers

term = number
   <|> variable
   <|> list
   <|> compoundTerm
   <|> atom
   <?> "term"

number = Number <$> natural
  where
    natural = toInteger <$> many1 digit

variable = Variable <$> variableIdentifier
  where
    variableIdentifier = (:) <$> upper <*> many wordChar

atom = Atom <$> identifier

compoundTerm = CompoundTerm <$> functor <*> subterms
  where
    functor  = identifier
    subterms = parens conjunction

-- Lists are translated on-the-fly into their full term representation
list = emptyList
   <|> brackets listContents

emptyList = Atom <$> reservedSym "[]"

listContents = makeListTerm <$> term <*> rest

  where

    rest = nextItem
       <|> tailItem
       <|> endOfList
       <?> "list contents"

    nextItem  = reservedSym "," *> listContents <?> "list item"
    tailItem  = reservedSym "|" *> term         <?> "tail item"
    endOfList = return Atom "[]"

    makeListTerm h t = CompoundTerm "." [h, t]


-- Token parsers

reservedWord s = lexeme $ try (string s) <* notFollowedBy wordChar
reservedSym  s = lexeme $ try (string s) <* notFollowedBy symChar

identifier = lexeme . unreserved $
      quotedIdentifier
  <|> unquotedIdentifier
  <|> symbolicIdentifier
  <?> "identifier"

quotedIdentifier = quotes "'" (many quotedChar)

unquotedIdentifier = (:) <$> lower <*> many wordChar

symbolicIdentifier = many1 symChar


unreserved p =
  do rslt <- p
     if rslt `elem` reserved
       then unexpected "reserved word or symbol"
       else return rslt


-- Lexical parsers

parens   = lexeme $ between (reservedSym "(") (reservedSym ")")
brackets = lexeme $ between (reservedSym "[") (reservedSym "]")
quotes q = lexeme $ between (reservedSym q)   (reservedSym q)

lexeme p = p <* whiteSpace

whiteSpace = skipMany space

quotedChar   = noneOf "'"
wordChar     = alphaNum
symChar      = oneOf "~`!@#$%^&*-_=+\\;:/?."


-- Miscellaneous

reserved = [":-", ",", "|"]
