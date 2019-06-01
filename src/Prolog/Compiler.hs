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
import Data.Functor.Identity (Identity)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Sequence (Seq, ViewL(..), (><))
import qualified Data.Sequence as Q
import Data.Set (Set)
import qualified Data.Set as S


data SimpleTerm = SVariable Identifier
                | SCompoundTerm Identifier [SimpleTerm]
                deriving (Eq, Ord, Show)

data SimpleClause = SDefiniteClause SimpleTerm [SimpleTerm]


runCompiler :: Writer (Seq WAM) a -> [WAM]
runCompiler = toList . execWriter


simplify :: HornClause -> Maybe SimpleClause
simplify (DefiniteClause head body) = Just (SDefiniteClause (simplifyAtoms head) (map simplifyAtoms body))
simplify (GoalClause _)             = Nothing

simplifyAtoms :: Term -> SimpleTerm
simplifyAtoms (Atom a)            = SCompoundTerm a []
simplifyAtoms (CompoundTerm f ts) = SCompoundTerm f (map simplifyAtoms ts)
simplifyAtoms (Number a)          = SCompoundTerm (show a) []
simplifyAtoms (Variable v)        = SVariable v


compileListing :: [HornClause] -> Program
compileListing listing = Program (map (uncurry compilePredicate) (M.toList predicates))

  where

    listing' = mapMaybe simplify listing

    predicates = foldr (\clause -> M.insertWith (++) (ftor clause) [clause]) M.empty listing'

    ftor (SDefiniteClause (SCompoundTerm f ts) _) = Functor f (length ts)
    ftor _ = undefined


-- | Compile a predicate into a sequence of WAM instructions.
compilePredicate :: Functor -> [SimpleClause] -> Predicate
compilePredicate f clauses = Predicate f (compileRules clauses)

compileRules :: [SimpleClause] -> [Rule]
compileRules []           = []
compileRules [singleton]  = [compileRule 0 (compileClause singleton)]
compileRules (first:rest) =
    zipWith compileRule [0..] $ compileFirst first : compileRest 1 rest

  where

    compileRest _ []          = undefined -- impossible
    compileRest _ [last]      = [compileLast last]
    compileRest n (next:rest) = compileMiddle n next : compileRest (n + 1) rest

    compileFirst clause =
      do emit (TryMeElse (Label 1))
         compileClause clause
    compileMiddle n clause =
      do emit (RetryMeElse (Label (n + 1)))
         compileClause clause
    compileLast clause =
      do emit TrustMe
         compileClause clause

compileRule :: Int -> Writer (Seq WAM) a -> Rule
compileRule n instructions = Rule (Label n) (runCompiler instructions)

-- | Compile a rule into a sequence of WAM instructions.
compileClause :: SimpleClause -> Writer (Seq WAM) ()
compileClause (SDefiniteClause head body) =
  evalStateT (compileClause' perms thead tbody) emptyRegisterSet
    where
      perms = sharedVars (head:body)
      (thead, tbody) = allocateClause perms head body
      compileClause' perms head body =
        do emit (Allocate (S.size perms))
           compileArgs Get head
           compileCall `mapM_` body
           emit Deallocate
           emit Proceed

emit :: MonadWriter (Seq a) m => a -> m ()
emit inst = tell (Q.singleton inst)


data Mode = Get | Put deriving (Eq, Ord, Show)

type RegisterSet = Set Register

emptyRegisterSet :: RegisterSet
emptyRegisterSet = S.empty

-- | Compile the arguments of a toplevel term in a clause.
compileArgs :: Mode -> TaggedTerm -> StateT RegisterSet (Writer (Seq WAM)) ()
compileArgs mode (TStructure _ _ args) = zipWithM_ (compileArg mode) [1..] args
compileArgs _    _                     = undefined -- TODO: clause heads should always be structures

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

compileStruct :: Mode -> FlattenedTerm -> StateT RegisterSet (Writer (Seq WAM)) ()
compileStruct mode (FStructure f reg subtermRegs) =
  do modify (S.insert reg)
     let functor = Functor f (length subtermRegs)
     case mode of
       Get -> emit (GetStructure functor reg)
       Put -> emit (PutStructure functor reg)
     compileUnify `mapM_` subtermRegs

compileUnify :: Register -> StateT RegisterSet (Writer (Seq WAM)) ()
compileUnify reg = ifEncountered reg (UnifyValue reg) (UnifyVariable reg)

ifEncountered :: Register -> WAM -> WAM -> StateT RegisterSet (Writer (Seq WAM)) ()
ifEncountered reg t f =
  do encountered <- gets (S.member reg)
     if encountered
       then emit t
       else
         do modify (S.insert reg)
            emit f

compileCall :: TaggedTerm -> StateT RegisterSet (Writer (Seq WAM)) ()
compileCall t@(TStructure f _ args) =
  do compileArgs Put t
     emit (Call (Functor f (length args)))
compileCall _ = undefined


type VariableMap = Map Identifier Int
type VariableSet = Set Identifier

-- NOTE: This skips the optimization that variables that occur only in the head
--   and the _first_ rule can be considered permanent. The special case didn't
--   seem worth the extra complication.
sharedVars :: [SimpleTerm] -> VariableSet
sharedVars terms = mconcat sharedVars'

  where

    sharedVars'  = evalState sharedVars'' M.empty
    sharedVars'' = zipWithM checkShared [1..] terms

    -- The state here maps a variable to the first top-level term in the rule
    -- where it was encountered.
    checkShared :: Int -> SimpleTerm -> State VariableMap VariableSet
    checkShared termNo (SCompoundTerm _ subterms) =
      mconcat <$> mapM (checkShared termNo) subterms
    checkShared termNo (SVariable v) =
      do firstEncountered <- gets (M.findWithDefault termNo v)
         if firstEncountered /= termNo
           then return (S.singleton v)
           else
             do modify (M.insert v termNo)
                return S.empty


data FlattenedTerm = FStructure Identifier Register [Register]

flatten :: Mode -> TaggedTerm -> [FlattenedTerm]
flatten Get = flattenGet
flatten Put = flattenPut

flattenPut :: TaggedTerm -> [FlattenedTerm]
flattenPut = reverse . flattenPut'
  where
    flattenPut' (TVariable _)             = []
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
            TVariable _             -> flattenGet' rest
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

emptyAllocation :: Allocation
emptyAllocation = Allocation 1 M.empty

reserveArgs :: Int -> Allocation
reserveArgs n   = Allocation (n+1) M.empty

allocateClause :: Traversable t => Set Identifier -> SimpleTerm -> t SimpleTerm -> (TaggedTerm, t TaggedTerm)
allocateClause  perms head body = evalState (allocateClause' perms head body) emptyAllocation
  where
    allocateClause' perms head body =
      (,) <$> allocateTop perms head <*> mapM (allocateTop perms) body

allocateTop :: Set Identifier -> SimpleTerm -> StateT Allocation Identity TaggedTerm
allocateTop perms (SCompoundTerm f args) = TStructure f register <$> args'
  where
    -- This register is a dummy value. The toplevel structure does not actually
    -- show up in data movement instructions (it only has to do with the predicate
    -- being called).
    register :: Register
    register = Register (-1)

    args' = evalStateT args'' (reserveArgs (length args))
    args'' = mapM (allocateArg perms) args
allocateTop _ _ = undefined


allocateArg :: VariableSet -> SimpleTerm -> StateT Allocation (State Allocation) TaggedTerm
allocateArg perms (SVariable v)              = TVariable <$> allocateVar perms v
allocateArg perms (SCompoundTerm f subterms) = TStructure f register <$> subterms'
  where
    register :: Register
    register = Register (-1)
    subterms' = mapM (allocate perms) subterms


allocate :: VariableSet -> SimpleTerm -> StateT Allocation (State Allocation) TaggedTerm
allocate perms (SVariable v)              = TVariable <$> allocateVar perms v
allocate perms (SCompoundTerm f subterms) = TStructure f <$> register <*> subterms'
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

  concrete (Program predicates) = intercalate "\n\n" (map concrete predicates)


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
  concrete Deallocate   = "deallocate"
  concrete (Call    f) = delim ["call",    concrete f]
  concrete (Execute f) = delim ["execute", concrete f]
  concrete Proceed     = "proceed"
  concrete (TryMeElse   l) = delim ["try_me_else",   concrete l]
  concrete (RetryMeElse l) = delim ["retry_me_else", concrete l]
  concrete TrustMe         = "trust_me"

delim :: [String] -> String
delim = intercalate "\t"


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
