-----------------------------------------------------------------------------
--
-- Module      :  Prolog.Data
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

module Prolog.Data (
  Identifier,
  Term(..),
  subterms,
  functor,
  arity,
  HornClause(..),
  Listing,

  Syntax,
  kind,
  concrete,
  describe,

  Tree(..),
  Operand(..),
  OpTable,
  OpDefinition(..),
  Associativity(..),
  Fixity(..),
  lbp,
  rbp,
  findOperator,

  InterpreterT,
  InterpreterState,
  listing,
  opTable,
  initialState,
  updateListing,
  updateOpTable,
  appendListing,
  insertOperator,
) where

import Control.Applicative ((<$>))
import Control.Monad.State
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S
import Data.Map (Map)
import qualified Data.Map as M


type Identifier = String

data Term = Atom Identifier
          | Number Integer
          | Variable Identifier
          | CompoundTerm Identifier [Term]
          deriving (Eq, Ord, Show)

subterms (CompoundTerm _ ts) = ts
subterms _                   = undefined

functor (CompoundTerm a _) = a
functor (Atom a)           = a
functor _                  = undefined

arity (CompoundTerm _ ts) = length ts
arity _                   = 0

data HornClause = DefiniteClause Term [Term]
                | GoalClause [Term]
                deriving (Eq, Ord, Show)

type Listing = Seq HornClause




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



-- Interpreter/parser state

type InterpreterT m = StateT InterpreterState m

data InterpreterState = InterpreterState {
                          opTable :: OpTable,
                          listing :: Listing
                        }
                      deriving (Eq, Ord, Show)

initialState :: InterpreterState
initialState = InterpreterState initialOpTable initialListing

initialOpTable :: OpTable
initialOpTable = M.fromList [
    ((":-",  Prefix),  OpDefinition Prefix  NonA   1200),
    (("?-",  Prefix),  OpDefinition Prefix  NonA   1200),
    ((":-",  Infix),   OpDefinition Infix   NonA   1200),
    ((",",   Infix),   OpDefinition Infix   RightA 1000),
    (("not", Prefix),  OpDefinition Prefix  NonA   900)
  ]

initialListing = S.empty

updateOpTable f = modify (\s -> s { opTable = f (opTable s) })

updateListing f = modify (\s -> s { listing = f (listing s) })

appendListing t = updateListing (|> t)


-- Operator-associated data structures

data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Eq, Ord, Show)


data Operand = Operator Identifier OpDefinition
             | Operand Term
             deriving (Eq, Ord, Show)

type OpTable = Map (Identifier, Fixity) OpDefinition

data OpDefinition = OpDefinition {
                      fixity        :: Fixity,
                      associativity :: Associativity,
                      precedence    :: Integer
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
lbp :: Operand -> Integer
lbp (Operand  _)     = 0
lbp (Operator _ def) = lbp' def
  where
    lbp' (OpDefinition Prefix _     _   ) = 0
    lbp' (OpDefinition _      LeftA prec) = prec + 1
    lbp' (OpDefinition _      _     prec) = prec

-- | Rightward binding precedence. Postfix operators and operands have an
--   RBP of zero (so that no operands will bind to their right). Right-
--   associative operators have an RBP of one greater than their normal
--   precedence (so that they bind operators with equal precedence to their
--   right).
rbp :: Operand -> Integer
rbp (Operand  _)     = 0
rbp (Operator _ def) = rbp' def
  where
    rbp' (OpDefinition Postfix _      _   ) = 0
    rbp' (OpDefinition _       RightA prec) = prec + 1
    rbp' (OpDefinition _       _      prec) = prec


findOperator fix a table = Operator a <$> (M.lookup (a, fix) table)

insertOperator a def = M.insert (a, fixity def) def
