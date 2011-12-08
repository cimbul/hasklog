module Compiler (

) where



-- WAM Instructions
  
data Register = Register Int
              | StackArg Int

newtype Label = Label { label :: Int }

data Functor = Functor Identifier Int

data WAM = GetStructure Functor
         | GetVariable  Register
         | GetValue     Register
         | PutStructure Functor  Register
         | PutVariable  Register Register
         | PutValue     Register Register
         | UnifyVariable Register
         | UnifyValue    Register
         | Allocate
         | Deallocate
         | Call    Functor Int
         | Execute Functor
         | Proceed
         | TryMeElse   Label
         | RetryMeElse Label
         | TrustMe


concrete' inst args = inst ++ "\t" ++ concreteArgs
  where concreteArgs = concat $ intersperse "\t" (concrete `map` args)
         
instance Syntax WAM where

  kind _ = "WAM instruction"
  
  concrete (GetStructure f) = concrete' "get_structure" [f]
  concrete (GetVariable  r) = concrete' "get_variable"  [r]
  concrete (GetValue     r) = concrete' "get_value"     [r]
  concrete (PutStructure f a) = concrete' "put_structure" [f, a]
  concrete (PutVariable  r a) = concrete' "put_variable"  [r, a]
  concrete (PutValue     r a) = concrete' "put_value"     [r, a]
  concrete (UnifyVariable r) = concrete' "unify_variable"  [r]
  concrete (UnifyValue    r) = concrete' "unify_value"     [r]
  concrete (Allocate)   = "allocate"
  concrete (Deallocate) = "deallocate"
  concrete (Call    f n) = concrete' "call"    [f, n]
  concrete (Execute f  ) = concrete' "execute" [f]
  concrete (Proceed)     = "proceed"
  concrete (TryMeElse   l) = concrete' "try_me_else" [l]
  concrete (RetryMeElse l) = concrete' "retry_me_else" [l]
  concrete (TrustMe)       = "trust_me"

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
  
  concrete (Functor f n) = show f ++ "/" ++ show n
