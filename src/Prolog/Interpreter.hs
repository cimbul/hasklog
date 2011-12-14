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
  interpret,
  program,

  resolve,
  unify,
) where


import Prolog.Data
import Prolog.Parser

import Control.Monad.List
import Control.Monad.State
import Control.Monad (foldM)
import Control.Applicative ((<$>), (<*>))
import Data.Maybe (mapMaybe, catMaybes)
import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Q
import Data.Map (Map)
import qualified Data.Map as M
import Text.Parsec hiding (State)


type Unifier = Map Identifier Term


type Predicate m = [Term] -> InterpreterT m [([Term], Unifier)]

interpret p = evalStateT p initialState



--
-- Builtins
--


-- | Look up the builtin predicate for a term. Returns @Nothing@ if one does not
--   exist.
builtin :: (MonadIO m, Functor m) => Term -> Maybe (Predicate m)
builtin t = M.lookup (functor t, arity t) builtins

-- | A map from a functor\/arity pair to a function that computes a builtin
--   operation
builtins :: (MonadIO m, Functor m) => Map (Identifier, Int) (Predicate m)
builtins = M.fromList [
    (("fail",    0), bfail),
    (("true",    0), btrue),
    (("not",     1), bnot),
    (("consult", 1), bconsult),
    (("op",      2), boperator)
  ]

-- | "Negation as a failure." Try to resolve the argument as a goal in the
--   program. Fail if any solutions are found; otherwise succeed.
bnot :: (MonadIO m, Functor m) => Predicate m
bnot [goal] =
  do unifiers <- resolve (GoalClause [goal])
     case unifiers of
       u:us -> bfail []
       []   -> btrue []

-- | Cause unfication to fail immediately.
bfail :: (MonadIO m) => Predicate m
bfail [] = return []

-- | Succeed without unifying anything or trigerring any further goals.
--   Strictly speaking, this doesn't have to be a builtin (it could just be
--   predefined as a fact), but having it as a function helps to write some
--   other builtins (ones that normally succeed, for instance.)
btrue :: (MonadIO m) => Predicate m
btrue [] = return [([], M.empty)]

-- | Add a user-defined operator to the parser's operator table.
boperator :: (MonadIO m, Functor m) => Predicate m
boperator [Number prec, Atom opType, Atom name] =
  do updateOpTable $ insertOperator name (opDef opType prec)
     btrue []

  where

    -- | Extract the fixity and associativity of an operator from a Prolog
    --   "operator type" atom ("fx", "xfy", etc.).
    opDef :: Identifier -> Integer -> OpDefinition
    opDef "xfx" = OpDefinition Infix   NonA
    opDef "xfy" = OpDefinition Infix   RightA
    opDef "yfx" = OpDefinition Infix   LeftA
    opDef "fx"  = OpDefinition Prefix  NonA
    opDef "fy"  = OpDefinition Prefix  RightA
    opDef "xf"  = OpDefinition Postfix NonA
    opDef "yf"  = OpDefinition Postfix LeftA
    opDef _     = undefined

bconsult :: (MonadIO m, Functor m) => Predicate m
bconsult [Atom filename] =
  do contents <- liftIO $ readFile filename
     result <- consult program filename contents
     case result of
       Left _  -> bfail []  -- TODO: Report error
       Right _ -> btrue []



--
-- Interactive program parsing
--


program :: (MonadIO m, Functor m) => PrologParser m [HornClause]
program = catMaybes <$> many rule

rule :: (MonadIO m, Functor m) => PrologParser m (Maybe HornClause)
rule =
  do c <- clause
     case c of
       -- Execute goal clauses and return the rest of the program
       g@(GoalClause _) ->
         do prog <- gets listing
            lift $ resolve g
            return Nothing
       -- Add definite clauses to the listing and return the clause followed by the
       -- rest of the program
       rule ->
         do appendListing rule
            return (Just rule)



--
-- Resolution
--


-- | Resolve the goal clause using the clauses in program. Return a list of all
--   possible unifiers.
resolve :: (MonadIO m, Functor m) => HornClause -> InterpreterT m [Unifier]
resolve (DefiniteClause _ _) = undefined
resolve (GoalClause goals)   = runListT $ resolve' goals M.empty

  where

    resolve' :: (MonadIO m, Functor m) => [Term] -> Unifier -> ListT (InterpreterT m) Unifier
    resolve' []        unifier = return unifier
    resolve' (g:goals) unifier =
      do (body, unifier') <- ListT $ predicate g
         let goals' = map (substituteAll unifier') (body ++ goals)
         resolve' goals' (M.union unifier unifier')

unifyClauses goal (DefiniteClause head body) =
  do unifier <- unify goal head
     return (body, unifier)

-- | "Call" a predicate (possibly builtin) and return all possible (/unifier/,
--   /body goal/) pairs.
predicate :: (MonadIO m, Functor m) => Term -> InterpreterT m [([Term], Unifier)]
predicate pred =
  case builtin pred of
    Just f  -> f (subterms pred)
    Nothing -> mapMaybe (unifyClauses pred) <$> gets (toList . listing)


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

substituteAll unification t = t


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

