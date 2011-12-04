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
  concrete,
  parseFully,
) where

import Prolog.Data

import Text.ParserCombinators.Parsec
import Control.Monad (guard, when)
import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Data.Map (Map)
import qualified Data.Map as M


parseFully p = parse (p <* eof)


program = many sentence

sentence = term <* fullStop


-- Term parsers

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

number = Number <$> natural
  where natural = read <$> many1 digit

fullStop = lexeme (string "." <* (layout <|> eof))

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

unquotedAtom = (:) <$> atomStart <*> many atomChar
  where
    atomStart = lower
    atomChar  = alphaNum <|> char '_'

quotedAtom = quotes '\'' (many quotedChar)
  where
    quotedChar = noneOf "'"

symbolicAtom =
  do sym <- many1 symbolicChar
     -- Make sure symbol is not a full stop (a period followed by layout)
     when (sym == ".") (notFollowedBy layout)
     return sym


--identifierChar = alphaNum <|> char '_'
symbolicChar   = oneOf "#$&*+-./:<=>?@\\^`~"


parens   = between (symbol "(") (symbol ")")
brackets = between (symbol "[") (symbol "]")
quotes q = between (char q)     (char q)

symbol s = lexeme (string s)
lexeme p = p <* many layout

layout = space   *> return ()
     <|> comment *> return ()

comment = symbol "%" *> many commentChar
  where
    commentChar = noneOf "\n"


-- Operator parers

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


operator fix maxPrecedence = operator' <?> describeFixity fix ++ " operator"
  where
    operator' =
      do a <- try atom
         Just op <- findOperator fix a <$> getState
         guard (maxPrecedence > lbp op)
         return op


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
  case parse (unquotedAtom <* eof) "" a of
    Left  _ -> True
    Right _ -> False


-- Operator-associated data structures

data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Eq, Ord, Show)


data Operand = Operator Identifier OpDefinition
             | Operand Term
             deriving (Eq, Ord, Show)

type OpTable = Map (Identifier, Fixity) OpDefinition

data OpDefinition = OpDefinition {
                      precedence    :: Int,
                      fixity        :: Fixity,
                      associativity :: Associativity
                    }
                  deriving (Eq, Ord, Show)

data Fixity = Prefix | Infix | Postfix
            deriving (Eq, Ord, Show)

data Associativity = NonA | LeftA | RightA
                   deriving (Eq, Ord, Show)


-- | Leftward binding precedence. Prefix and operators and operands have an
--   LBP of zero (so that no operands will bind to their left). Left-
--   associative operators have an LBP of one greater than their normal
--   precedence (so that they bind operators with equal precedence to their
--   right).
lbp :: Operand -> Int
lbp (Operand  _)     = 0
lbp (Operator _ def) = lbp' def
  where
    lbp' (OpDefinition _    Prefix _    ) = 0
    lbp' (OpDefinition prec _      LeftA) = prec + 1
    lbp' (OpDefinition prec _      _    ) = prec

-- | Rightward binding precedence. Postfix operators and operands have an
--   RBP of zero (so that no operands will bind to their right). Right-
--   associative operators have an RBP of one greater than their normal
--   precedence (so that they bind operators with equal precedence to their
--   right).
rbp :: Operand -> Int
rbp (Operand  _)     = 0
rbp (Operator _ def) = rbp' def
  where
    rbp' (OpDefinition _    Postfix _     ) = 0
    rbp' (OpDefinition prec _       RightA) = prec + 1
    rbp' (OpDefinition prec _       _     ) = prec


findOperator fix a table = Operator a <$> (M.lookup (a, fix) table)



describeFixity Infix   = "infix"
describeFixity Prefix  = "prefix"
describeFixity Postfix = "postfix"


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


instance Syntax Operand where

  kind (Operand  _)     = "operand"
  kind (Operator _ def) =
    case fixity def of
      Infix   -> "infix operator"
      Prefix  -> "prefix operator"
      Postfix -> "postfix operator"

  concrete (Operand  t)   = concrete t
  concrete (Operator a _) = concrete (Atom a)
