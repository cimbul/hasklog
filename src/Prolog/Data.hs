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
  HornClause(..),
  Program,
) where

type Identifier = String

data Term = Atom Identifier
          | Number Integer
          | Variable Identifier
          | CompoundTerm Identifier [Term]
          deriving (Eq, Ord, Show)

data HornClause = DefiniteClause Term [Term]
                | GoalClause [Term]
                deriving (Eq, Ord, Show)

type Program = [HornClause]
