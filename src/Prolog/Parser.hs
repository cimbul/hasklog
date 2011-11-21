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
  parse,   -- reexport from Parsec
  parseFully,

  program,
  query,
  clause,

  concreteSyntax,
  quoteAtom,
) where

import Prolog.Data

import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Control.Monad (guard)
import Control.Monad.Reader
import Text.ParserCombinators.Parsec

import Data.List (intersperse)
import qualified Data.Map as M


parseFully p = parse (p <* eof)


-- Program/Query parsers

query = GoalClause <$> goal
  where
    goal = conjunction <* (reservedSym ".")

program = clauses

clauses = clause `endBy` reservedSym "."

clause = DefiniteClause <$> clauseHead <*> clauseBody
  where
    clauseHead = term
    clauseBody = option [] (reservedSym ":-" *> conjunction)

conjunction = term `sepBy` reservedSym ","


-- Term parsers

term = number
   <|> variable
   <|> list
   <|> atomOrCompound
   <?> "term"

number = Number <$> natural
  where
    natural = (read :: String -> Integer) <$> many1 digit

variable = Variable <$> variableIdentifier
  where
    variableIdentifier = (:) <$> upper <*> many wordChar

atomOrCompound = compoundTerm >>= translate
  where
    -- Translate compound terms with 0 arity into atoms
    translate compound =
      case compound of
        CompoundTerm f [] -> return (Atom f)
        _                 -> return compound

atom = Atom <$> identifier

compoundTerm = CompoundTerm <$> functor <*> subterms
  where
    functor  = identifier
    subterms = option [] (parens conjunction)

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
    endOfList = return (Atom "[]")

    makeListTerm h t = CompoundTerm "." [h, t]


-- Token parsers

reservedWord s = lexeme $ try (string s) <* notFollowedBy wordChar
reservedSym  s = lexeme $ try (string s) <* notFollowedBy symChar

identifier = lexeme (unreserved (
      quotedIdentifier
  <|> unquotedIdentifier
  <|> symbolicIdentifier
  <?> "identifier"))

quotedIdentifier = quotes "'" (many quotedChar)

unquotedIdentifier = (:) <$> lower <*> many wordChar

symbolicIdentifier = many1 symChar


unreserved p = p >>= checkReserved
  where
    checkReserved rslt =
     if rslt `elem` reserved
       then unexpected ("reserved word or symbol \"" ++ rslt ++ "\"")
       else return rslt


-- Lexical parsers

parens   p = lexeme (between (symbol "(") (symbol ")") p)
brackets p = lexeme (between (symbol "[") (symbol "]") p)
quotes q p = lexeme (between (symbol q)   (symbol q)   p)

symbol s = lexeme (string s)

lexeme p = p <* spaces

quotedChar   = noneOf "'"
wordChar     = alphaNum <|> char '_'
symChar      = oneOf "+-*/<>=:.&_~"


-- Miscellaneous

reserved = [":-", ",", "|"]


-- | Convert an AST term to concrete syntax.
concreteSyntax (Atom a)     = quoteAtom a
concreteSyntax (Number n)   = show n
concreteSyntax (Variable v) = v
concreteSyntax (CompoundTerm f ts) =
    quoteAtom f ++ "(" ++ subterms ++ ")"
  where
    subterms = concat (intersperse "," (map concreteSyntax ts))


quoteAtom a =
  case parse unquotedIdentifier "" a of
    Left _  -> quote a
    Right _ -> a
  where
    quote a = "'" ++ a ++ "'"


-- Operators

data OpFixity = Infix | Postfix | Prefix
              deriving (Eq, Ord, Show)

data OpAssociativity = NonA | LeftA | RightA
                     deriving (Eq, Ord, Show)

data OpDefinition = OpDefinition Int OpFixity OpAssociativity
                  deriving (Eq, Ord, Show)

type OpTable = M.Map Identifier [OpDefinition]


getOpDef opTable ident = maybe defaultOpDef id $ M.lookup ident opTable

defaultOpDef = OpDefinition defaultBinding Infix NonA
defaultBinding = 1500

lbp (Atom a) opTable = lbp' (getOpDef opTable a)
lbp _        opTable = defaultBinding

rbp (Atom a) opTable = rbp' (getOpDef opTable a)
rbp _        opTable = defaultBinding

-- | Leftward binding precedence
lbp' (OpDefinition bp _ LeftA) = bp
lbp' (OpDefinition bp _ _    ) = bp - 1 -- FIXME: Does this work?

-- | Rightward binding precedence
rbp' (OpDefinition bp _ RightA) = bp - 1
rbp' (OpDefinition bp _ _     ) = bp



data Tree a = Node a (Tree a) (Tree a)
            | Empty

type Token = Term

-- TODO: Error checking?
opTreeToTerm :: Tree Term -> Term
opTreeToTerm Empty        = undefined
opTreeToTerm (Node (Atom a) l r) =
  case (l, r) of
    (Node _ _ _, Node _ _ _) -> CompoundTerm a [(opTreeToTerm l), (opTreeToTerm r)]
    (Node _ _ _, Empty)      -> CompoundTerm a [(opTreeToTerm l)]
    (Empty     , Node _ _ _) -> CompoundTerm a [(opTreeToTerm r)]
    (Empty     , Empty)      -> Atom a
opTreeToTerm (Node t l r) = t

opTree = opTree' 0 Empty
  where
    opTree' minbp currentTree = treeWithNextTerm minbp currentTree
                            <|> return currentTree

    treeWithNextTerm minbp leftTree =
      do next <- try $ termWithAtLeast minbp
         termRbp <- rbp next <$> getState
         rightTree <- opTree' termRbp Empty
         opTree' minbp (Node next leftTree rightTree)

    termWithAtLeast minbp =
      do tok <- term
         termLbp <- lbp tok <$> getState
         guard $ minbp < termLbp
         return tok
