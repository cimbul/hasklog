-----------------------------------------------------------------------------
--
-- Module      :  Prolog.Compiler
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

{-# LANGUAGE FlexibleContexts #-}

module Prolog.Compiler (
  compileListing
) where

import Prolog.Data

import Prelude hiding (Functor)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Writer hiding (Functor)
import Control.Monad.State hiding (Functor)
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq, ViewL(..), (><))
import qualified Data.Sequence as Q
import Data.Set (Set)
import qualified Data.Set as S


runCompiler = toList . execWriter


simplify (DefiniteClause head body) = DefiniteClause (simplifyAtoms head) (map simplifyAtoms body)

simplifyAtoms (Atom a)            = CompoundTerm a []
simplifyAtoms (CompoundTerm f ts) = CompoundTerm f (map simplifyAtoms ts)
simplifyAtoms t                   = t


compileListing :: [HornClause] -> Program
compileListing listing = Program (map (uncurry compilePredicate) (M.toList predicates))

  where

    listing' = map simplify listing

    predicates = foldr (\clause -> M.insertWith (++) (ftor clause) [clause]) M.empty $ listing'

    ftor (DefiniteClause head body) = Functor (functor head) (arity head)


-- | Compile a predicate into a sequence of WAM instructions.
compilePredicate :: Functor -> [HornClause] -> Predicate
compilePredicate f clauses = Predicate f (compileRules clauses)

compileRules :: [HornClause] -> [Rule]
compileRules [singleton]  = [compileRule 0 (compileClause singleton)]
compileRules (first:rest) =
    zipWith compileRule [0..] $ compileFirst first : compileRest 1 rest

  where

    compileRest n [last]      = [compileLast n last]
    compileRest n (next:rest) = compileMiddle n next : compileRest (n + 1) rest

    compileFirst clause =
      do emit (TryMeElse (Label 1))
         compileClause clause
    compileMiddle n clause =
      do emit (RetryMeElse (Label (n + 1)))
         compileClause clause
    compileLast n clause =
      do emit (TrustMe)
         compileClause clause

compileRule n instructions = Rule (Label n) (runCompiler instructions)

-- | Compile a rule into a sequence of WAM instructions.
compileClause :: HornClause -> Writer (Seq WAM) ()
compileClause (DefiniteClause head body) =
  let perms = sharedVars (head:body)
      (thead, tbody) = allocateClause perms head body
   in evalStateT (compileClause' perms thead tbody) emptyRegisterSet

compileClause' perms head body =
  do emit (Allocate (S.size perms))
     compileArgs Get head
     compileCall`mapM` body
     emit (Deallocate)
     emit (Proceed)

emit inst = tell (Q.singleton inst)


data Mode = Get | Put deriving (Eq, Ord, Show)

type RegisterSet = Set Register

emptyRegisterSet = S.empty

-- | Compile the arguments of a toplevel term in a clause.
compileArgs :: Mode -> TaggedTerm -> StateT RegisterSet (Writer (Seq WAM)) ()
compileArgs mode (TStructure _ _ args) = zipWithM_ (compileArg mode) [1..] args

compileArg :: Mode -> Int -> TaggedTerm -> StateT RegisterSet (Writer (Seq WAM)) ()
compileArg mode n (TVariable reg) =
  let arg = Register n
   in case mode of
        Get -> ifEncountered reg (GetValue reg arg) (GetVariable reg arg)
        Put -> ifEncountered reg (PutValue reg arg) (PutVariable reg arg)
compileArg mode n (TStructure f _ subterms) =
  let -- Replace the register on the argument structure (which is a dummy value in this case) with
      -- the argument register. Then flatten it and compile the resulting expressions.
      flattened = flatten mode (TStructure f (Register n) subterms)
   in compileStruct mode `mapM_` flattened

compileStruct mode (FStructure f reg subtermRegs) =
  do modify (S.insert reg)
     let functor = Functor f (length subtermRegs)
     case mode of
       Get -> emit (GetStructure functor reg)
       Put -> emit (PutStructure functor reg)
     compileUnify `mapM_` subtermRegs

compileUnify reg = ifEncountered reg (UnifyValue reg) (UnifyVariable reg)

ifEncountered reg t f =
  do encountered <- gets (S.member reg)
     if encountered
       then emit t
       else
         do modify (S.insert reg)
            emit f

compileCall t@(TStructure f _ args) =
  do compileArgs Put t
     emit (Call (Functor f (length args)))


type VariableMap = Map Identifier Int
type VariableSet = Set Identifier

-- NOTE: This skips the optimization that variables that occur only in the head
--   and the _first_ rule can be considered permanent. The special case didn't
--   seem worth the extra complication.
sharedVars :: [Term] -> VariableSet
sharedVars terms = mconcat sharedVars'

  where

    sharedVars'  = evalState sharedVars'' M.empty
    sharedVars'' = zipWithM checkShared [1..] terms

    -- The state here maps a variable to the first top-level term in the rule
    -- where it was encountered.
    checkShared :: Int -> Term -> State VariableMap VariableSet
    checkShared termNo (CompoundTerm f subterms) =
      mconcat <$> mapM (checkShared termNo) subterms
    checkShared termNo (Variable v) =
      do firstEncountered <- gets (M.findWithDefault termNo v)
         if firstEncountered /= termNo
           then return (S.singleton v)
           else
             do modify (M.insert v termNo)
                return (S.empty)


data FlattenedTerm = FStructure Identifier Register [Register]

flatten Get = flattenGet
flatten Put = flattenPut

flattenPut :: TaggedTerm -> [FlattenedTerm]
flattenPut = reverse . flattenPut'
  where
    flattenPut' (TVariable v)             = []
    flattenPut' (TStructure f n subterms) =
      FStructure f n (map tag subterms) : concatMap flattenPut' subterms


flattenGet :: TaggedTerm -> [FlattenedTerm]
flattenGet t = flattenGet' (Q.singleton t)
  where
    flattenGet' queue =
      case Q.viewl queue of
        EmptyL        -> []
        first :< rest ->
          case first of
            TVariable n             -> flattenGet' rest
            TStructure f n subterms ->
              FStructure f n (map tag subterms) : flattenGet' (rest >< Q.fromList subterms)


data TaggedTerm = TVariable Register
                | TStructure Identifier Register [TaggedTerm]

tag :: TaggedTerm -> Register
tag (TVariable t)      = t
tag (TStructure _ t _) = t


data Allocation = Allocation {
                    nextFree  :: Int,
                    allocated :: VariableMap
                  }
                deriving (Eq, Ord, Show)

emptyAllocation = Allocation 1 M.empty
reserveArgs n   = Allocation (n+1) M.empty

allocateClause  perms head body = evalState (allocateClause' perms head body) emptyAllocation
allocateClause' perms head body =
   (,) <$> allocateTop perms head <*> mapM (allocateTop perms) body

allocateTop perms (CompoundTerm f args) = TStructure f register <$> args'
  where
    -- This register is a dummy value. The toplevel structure does not actually
    -- show up in data movement instructions (it only has to do with the predicate
    -- being called).
    register :: Register
    register = Register (-1)

    args' = evalStateT args'' (reserveArgs (length args))
    args'' = zipWithM (allocateArg perms) [1..] args


allocateArg perms n (Variable v)              = TVariable <$> allocateVar perms v
allocateArg perms n (CompoundTerm f subterms) = TStructure f register <$> subterms'
  where
    register :: Register
    register = Register (-1)
    subterms' = mapM (allocate perms) subterms


allocate :: VariableSet -> Term -> StateT Allocation (State Allocation) TaggedTerm
allocate perms (Variable v)              = TVariable <$> allocateVar perms v
allocate perms (CompoundTerm f subterms) = TStructure f <$> register <*> subterms'
  where
    register = do Allocation next vars <- get
                  put (Allocation (next + 1) vars)
                  return (Register next)
    subterms' = mapM (allocate perms) subterms


-- | Find the storage associated with a variable. Allocate a new location if necessary. The location
--   will be a stack variable if the variable is permanent, or a register if it is temporary.
allocateVar :: VariableSet -> Identifier -> StateT Allocation (State Allocation) Register
allocateVar permanents v =
    -- Determine whether this is a temporary or permanent variable, and construct an appropriate
    -- Register type with the allocated variable.
    if S.member v permanents
      then StackVar <$> lift (allocateVar' v)
      else Register <$> allocateVar' v
  where
    -- Find the register (number) associated with a variable. A new one will be allocated if
    -- necessary.
    allocateVar' v =
      do Allocation next vars <- get
         case M.lookup v vars of
           Just register -> return register
           Nothing ->
             do put (Allocation (next + 1) (M.insert v next vars))
                return next



-- WAM Instructions

newtype Program = Program [Predicate]

data Predicate = Predicate Functor [Rule]

data Rule = Rule Label [WAM]

data WAM = GetStructure Functor  Register
         | GetVariable  Register Register
         | GetValue     Register Register
         | PutStructure Functor  Register
         | PutVariable  Register Register
         | PutValue     Register Register
         | UnifyVariable Register
         | UnifyValue    Register
         | Allocate   Int
         | Deallocate
         | Call    Functor
         | Execute Functor
         | Proceed
         | TryMeElse   Label
         | RetryMeElse Label
         | TrustMe
         deriving (Eq, Ord, Show)

data Register = Register Int
              | StackVar Int
              deriving (Eq, Ord, Show)

newtype Label = Label Int
              deriving (Eq, Ord, Show)

data Functor = Functor Identifier Int
             deriving (Eq, Ord, Show)


instance Syntax Program where

  kind _ = "program"

  describe = kind

  concrete (Program predicates) = concat (intersperse "\n\n" (map concrete predicates))


instance Syntax Predicate where

  kind _ = "predicate"

  describe (Predicate f _) = "predicate " ++ concrete f

  concrete (Predicate f rules) = concrete f ++ ":" ++ concreteRules
    where
      concreteRules = concatMap (\r -> "\n\t" ++ concrete r) rules



instance Syntax Rule where

  kind _ = "rule"

  describe (Rule label _) = "rule " ++ concrete label

  concrete (Rule label instructions) = concrete label ++ ":" ++ concreteInsts
    where
      concreteInsts = concatMap (\i -> "\n\t\t" ++ concrete i) instructions



instance Syntax WAM where

  kind _ = "WAM instruction"

  concrete (GetStructure f a) = delim ["get_structure", concrete f, concrete a]
  concrete (GetVariable  r a) = delim ["get_variable",  concrete r, concrete a]
  concrete (GetValue     r a) = delim ["get_value",     concrete r, concrete a]
  concrete (PutStructure f a) = delim ["put_structure", concrete f, concrete a]
  concrete (PutVariable  r a) = delim ["put_variable",  concrete r, concrete a]
  concrete (PutValue     r a) = delim ["put_value",     concrete r, concrete a]
  concrete (UnifyVariable r) = delim ["unify_variable", concrete r]
  concrete (UnifyValue    r) = delim ["unify_value",    concrete r]
  concrete (Allocate n) = delim ["allocate", show n]
  concrete (Deallocate) = "deallocate"
  concrete (Call    f) = delim ["call",    concrete f]
  concrete (Execute f) = delim ["execute", concrete f]
  concrete (Proceed)     = "proceed"
  concrete (TryMeElse   l) = delim ["try_me_else",   concrete l]
  concrete (RetryMeElse l) = delim ["retry_me_else", concrete l]
  concrete (TrustMe)       = "trust_me"

delim elems = concat $ intersperse "\t" elems


instance Syntax Label where

  kind _ = "label"

  concrete (Label l) = show l


instance Syntax Register where

  kind (Register _) = "register"
  kind (StackVar _) = "stack variable"

  concrete (Register r) = "X" ++ show r
  concrete (StackVar v) = "Y" ++ show v


instance Syntax Functor where

  kind _ = "functor"

  concrete (Functor f n) = f ++ "/" ++ show n
