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
  program,
  term,
  parseFully,

  Syntax,
  concrete,
  describe,
  kind,

  PrologParser
) where

import Prolog.Data

import Text.Parsec hiding (Empty, State)
import Control.Monad (guard, when)
import Control.Monad.State
import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Data.Maybe
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S


type PrologParser a = ParsecT String () (State InterpreterState) a


parseFully :: PrologParser a -> SourceName -> String -> Either ParseError a
parseFully p n s = evalState (runParserT (p <* eof) () n s) initialState


program :: PrologParser [Term]
program = many sentence

sentence :: PrologParser Term
sentence = term <* fullStop


-- Term parsers

term :: PrologParser Term
term = operation <* notFollowedBy primitiveTerm

primitiveTerm = parens term
            <|> variable
            <|> number
            <|> list
            <|> compoundOrAtom
            <?> "term"

compoundOrAtom = simplify <$> compound
  where
    simplify (CompoundTerm a []) = Atom a
    simplify t                   = t

compound = CompoundTerm <$> functor <*> subterms
  where
    functor  = rawAtom
    subterms = option [] (parens (term `sepBy` symbol ","))

list = emptyList
   <|> brackets listContents

  where

    emptyList = Atom <$> try (symbol "[]")

    listContents = listTerm <$> term <*> rest

    rest = nextItem
       <|> tailItem
       <|> endOfList

    nextItem  = symbol "," *> (listTerm <$> term <*> rest)
    tailItem  = symbol "|" *> term
    endOfList = return (Atom "[]")

    listTerm h t = CompoundTerm "." [h,t]


-- Token parsers

fullStop :: PrologParser String
fullStop = lexeme (string "." <* (layout <|> eof))

number = Number <$> natural
  where natural = read <$> many1 digit

variable = Variable <$> lexeme rawVariable

rawVariable = (:) <$> variableStart <*> many variableChar
  where
    variableStart = upper
    variableChar  = alphaNum <|> char '_'

atom = lexeme rawAtom

rawAtom = quotedAtom
      <|> unquotedAtom
      <|> symbolicAtom
      <?> "atom"

unquotedAtom :: PrologParser String
unquotedAtom = (:) <$> atomStart <*> many atomChar
  where
    atomStart = lower
    atomChar  = alphaNum <|> char '_'

quotedAtom :: PrologParser String
quotedAtom = quotes '\'' (many quotedChar)
  where
    quotedChar = noneOf "'"

symbolicAtom :: PrologParser String
symbolicAtom =
  do sym <- many1 symbolicChar
     -- Make sure symbol is not a full stop (a period followed by layout)
     when (sym == ".") (notFollowedBy layout)
     return sym
  where
    symbolicChar = oneOf "#$&*+-./:<=>?@\\^`~"


parens   = between (symbol "(") (symbol ")")
brackets = between (symbol "[") (symbol "]")
quotes q = between (char q)     (char q)

symbol :: String -> PrologParser String
symbol s = lexeme (string s)
lexeme :: PrologParser a -> PrologParser a
lexeme p = p <* many layout

layout :: PrologParser ()
layout = space   *> return ()
     <|> comment *> return ()

comment = symbol "%" *> many commentChar
  where
    commentChar = noneOf "\n"


-- Operator parers

operation :: PrologParser Term
operation = toTerm <$> operatorTree 1201 Empty

  where

    toTerm (Node (Operand t)      _     _    ) = t
    toTerm (Node (Operator a def) ltree rtree) =
      case (ltree, rtree) of
        (Node _ _ _, Node _ _ _) -> toCompound a [ltree, rtree]
        (Node _ _ _, Empty     ) -> toCompound a [ltree]
        (Empty     , Node _ _ _) -> toCompound a [rtree]
        (Empty     , Empty     ) -> Atom a

    toCompound a subtrees = CompoundTerm a (map toTerm subtrees)

operatorTree :: Integer -> Tree Operand -> PrologParser (Tree Operand)
operatorTree maxPrecedence left = continue <|> end

  where

    continue = current >>= operatorTree maxPrecedence

    end = do guard (left /= Empty)
             return left

    current =
      case left of
        Empty -> try prefixOperation <|> operand
        _     -> try infixOperation  <|> try postfixOperation

    prefixOperation  = operator Prefix  maxPrecedence >>= withRight
    infixOperation   = operator Infix   maxPrecedence >>= withRight
    postfixOperation = operator Postfix maxPrecedence >>= withEmpty
    operand          = Operand <$> primitiveTerm      >>= withEmpty

    withEmpty op = return (Node op left Empty)

    withRight op = Node op left <$> right
      where
        right = operatorTree (rbp op) Empty <?> "argument for " ++ describe op


operator :: Fixity -> Integer -> PrologParser Operand
operator fix maxPrecedence = operator' <?> describeFixity fix ++ " operator"
  where
    operator' =
      do a <- try atom
         Just op <- findOperator fix a <$> gets opTable
         guard (maxPrecedence > lbp op)
         return op



-- Data structures

-- | An abstract syntax element. Has methods for describing the syntax in
--   output. Minimum complete definition: @concrete@ and @kind@.
class Syntax s where

  -- | Convert an abstract syntax element into concrete syntax.
  concrete :: s -> String

  -- | Get a string describing the kind of syntax element (e.g., a term). Used
  --   by the default definition of @describe@.
  kind :: s -> String

  -- | Describe the syntax element in human-readable terms, e.g., for an error
  --   message. The default implementation uses the kind of term and its
  --   concrete syntax.
  describe :: s -> String
  describe s = kind s ++ " " ++ concrete s




-- Term data structures

instance Syntax Term where

  kind _ = "term"

  concrete (Number n) = show n
  concrete (Atom a)   = quoteAtom a
  concrete (CompoundTerm a subterms) =
    quoteAtom a ++ "(" ++ concatMap concrete subterms ++ ")"

quoteAtom a =
  if needsQuotes a
    then "'" ++ a ++ "'"
    else a

needsQuotes a =
  case parseFully unquotedAtom "" a of
    Left  _ -> True
    Right _ -> False



describeFixity Infix   = "infix"
describeFixity Prefix  = "prefix"
describeFixity Postfix = "postfix"


instance Syntax Operand where

  kind (Operand  _)     = "operand"
  kind (Operator _ def) =
    case fixity def of
      Infix   -> "infix operator"
      Prefix  -> "prefix operator"
      Postfix -> "postfix operator"

  concrete (Operand  t)   = concrete t
  concrete (Operator a _) = concrete (Atom a)
