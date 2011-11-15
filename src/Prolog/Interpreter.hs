-----------------------------------------------------------------------------
--
-- Module      :  Prolog.Interpreter
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

module Prolog.Interpreter (
  unify,
) where


import Prolog.Data

import Control.Monad (foldM)
import qualified Data.Map as M


type Unifier = M.Map Identifier Term


-- | Resolve the goal clause using the clauses in program. Return a list of all
--   possible unifiers.
resolve :: Program -> HornClause -> [Unifier]

resolve prog (GoalClause goals) = resolve' prog goals M.empty

  where

    resolve' prog []      unifier = return unifier
    resolve' prog g:goals unifier =
      do (body, unifier') <- mapMaybe (unifyClauses g) prog
         let goals' = map (substituteAll unifier') (body ++ goals)
         resolve' prog goals' (union unifier unifier')

    unifyClauses goal (DefiniteClause head body) =
      do unifier' <- unify goal head
         return (body, unifier)

-- Cannot resolve with a definite clause
resolve prog _ = undefined


-- | "Occurs check": Check whether a variable occurs in a compound term.
--   A variable cannot be unified with a compound term that contains it --
--   otherwise an infinite expression would result.
occurs :: Term -> Term -> Bool
occurs (Variable var) (Variable var')       = var == var'
occurs (Variable var) (CompoundTerm _ args) = any (occurs (Variable var)) args
occurs (Variable var) _                     = False
occurs _              _                     = undefined


-- | Attempt to unify the two terms. If unification fails, return Nothing.
--   Otherwise, return the most general unifier, a list of unique variable
--   substitutions.
unify :: Term -> Term -> Maybe Unifier
unify a b = unify' a b M.empty

  where

    unify' :: Term -> Term -> Unifier -> Maybe Unifier

    -- Variables unify with anything, so long as they do not occur in the term
    -- they are unified with. If both terms are identical variables, they can
    -- be skipped.
    unify' a@(Variable var) b unifier
      | a == b     = Just unifier
      | occurs a b = Nothing
      | otherwise  =
          let unifier' = M.map (substitute a b) unifier
           in Just (M.insert var b unifier')

    -- Flip the direction if b is a variable but a is not.
    unify' a b@(Variable var) unifier =
      unify' b a unifier

    -- Compound terms unify with each other iff their functors and arities are
    -- identical and their subterms unify *simultaneously.*
    unify' a@(CompoundTerm f ts) b@(CompoundTerm f' ts') unifier
      | f /= f'                 = Nothing
      | length ts /= length ts' = Nothing
      | otherwise =
            foldM substituteAndUnify unifier (zip ts ts')
          where
            substituteAndUnify unifier termPair =
              (uncurry unify') (substituteAll unifier <$> termPair) unifier

    -- Other terms unify iff they are identical
    unify' a b unifier
      | a == b    = Just unifier
      | otherwise = Nothing


-- | For every variable in the term which has a substitution in the unifier,
--   substitute the variable for that value.
substituteAll :: Unifier -> Term -> Term

substituteAll unification v@(Variable var) =
  case M.lookup var unification of
    Just sub -> sub
    Nothing  -> v

substituteAll unification (CompoundTerm f ts) =
  CompoundTerm f (map (substituteAll unification) ts)

substutiteAll unification t = t


-- | Substitute each occurance of the first term (a variable) by the second term
--   in the subject (third) term.
substitute :: Term -> Term -> Term -> Term

substitute v@(Variable var) replacement orig@(Variable var')
  | var == var' = replacement
  | otherwise   = orig

substitute v@(Variable var) replacement orig@(CompoundTerm f params) =
  CompoundTerm f (map (substitute v replacement) params)

substitute v@(Variable var) _ orig = orig
substitute _                _ _    = undefined

