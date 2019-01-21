module Prolog.Parser (
  sentence,
  clause,
  term,

  consult,
  parse,
  parseTest,

  PrologParser
) where

import Prolog.Data

import Text.Parsec hiding (Empty, State, parse, parseTest)
import Control.Monad (guard, when)
import Control.Monad.State
import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Data.List (intercalate)
import Data.Functor (($>))
import Data.Functor.Identity
import Data.Maybe
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S


type PrologParser m = ParsecT String () (InterpreterT m)


consult :: (Monad m) => PrologParser m a -> SourceName -> String -> InterpreterT m (Either ParseError a)
consult p = runParserT (many layout *> p <* eof) ()

parseTest p input = runIdentity $ parse p "" input

parse :: (Monad m) => PrologParser m a -> SourceName -> String -> m (Either ParseError a)
parse p n s = evalStateT (consult p n s) initialState

sentence :: Monad m => PrologParser m Term
sentence = term <* fullStop


clause :: Monad m => PrologParser m HornClause
clause = toClause <$> sentence

  where

    toClause (CompoundTerm ":-" [head, body]) = DefiniteClause head (toConjunction body)
    toClause (CompoundTerm ":-" [body])       = GoalClause (toConjunction body)
    toClause (CompoundTerm "?-" [body])       = GoalClause (toConjunction body)
    toClause fact                             = DefiniteClause fact []

    -- | Convert any (possibly nested) conjunction terms into a list of terms.
    toConjunction (CompoundTerm "," [t1, t2]) = t1 : toConjunction t2
    toConjunction t                           = [t]


-- Term parsers

term :: Monad m => PrologParser m Term
term = operation 1200 <* notFollowedBy primitiveTerm

primitiveTerm :: Monad m => PrologParser m Term
primitiveTerm = parens term
            <|> variable
            <|> number
            <|> list
            <|> compoundOrAtom
            <?> "term"

argument :: Monad m => PrologParser m Term
argument = operation 999

compoundOrAtom :: Monad m => PrologParser m Term
compoundOrAtom = simplify <$> lexeme compound
  where
    simplify (CompoundTerm a []) = Atom a
    simplify t                   = t

compound :: Monad m => PrologParser m Term
compound = CompoundTerm <$> functor <*> subterms
  where
    functor :: Monad m => PrologParser m String
    functor  = rawAtom
    subterms = option [] (parens (argument `sepBy` symbol ","))

list :: Monad m => PrologParser m Term
list = emptyList
   <|> brackets listContents

  where


    listContents = listTerm <$> argument <*> rest

    rest = nextItem
       <|> tailItem
       <|> endOfList

    nextItem  = symbol "," *> listContents
    tailItem  = symbol "|" *> argument
    endOfList = return (Atom "[]")

    listTerm h t = CompoundTerm "." [h,t]

emptyList :: Monad m => PrologParser m Term
emptyList = Atom <$> try (symbol "[]")

-- Token parsers

fullStop :: Monad m => PrologParser m String
fullStop = lexeme (string "." <* (layout <|> eof))

number :: Monad m => PrologParser m Term
number = Number <$> lexeme natural
  where natural = read <$> many1 digit

variable :: Monad m => PrologParser m Term
variable = Variable <$> lexeme rawVariable

rawVariable :: Monad m => PrologParser m String
rawVariable = (:) <$> variableStart <*> many variableChar
  where
    variableStart = upper
    variableChar  = alphaNum <|> char '_' <?> "rest of variable"

atom :: Monad m => PrologParser m String
atom = lexeme rawAtom

nonFunctorAtom :: Monad m => PrologParser m String
nonFunctorAtom = lexeme (rawAtom <* notFollowedBy (symbol "("))

rawAtom :: Monad m => PrologParser m String
rawAtom = quotedAtom
      <|> unquotedAtom
      <|> symbolicAtom
      <|> reservedSymbol
      <?> "atom"

unquotedAtom :: Monad m => PrologParser m String
unquotedAtom = (:) <$> atomStart <*> many atomChar
  where
    atomStart = lower
    atomChar  = alphaNum <|> char '_' <?> "rest of atom"

quotedAtom :: Monad m => PrologParser m String
quotedAtom = quotes '\'' (many quotedChar)
  where
    quotedChar = noneOf "'"

symbolicAtom :: Monad m => PrologParser m String
symbolicAtom =
  do sym <- many1 symbolicChar
     -- Make sure symbol is not a full stop (a period followed by layout or end of input)
     when (sym == ".") (lookAhead anyToken >> notFollowedBy layout)
     return sym
  where
    symbolicChar = oneOf "#$&*+-./:<=>?@\\^`~"

reservedSymbol :: Monad m => PrologParser m String
reservedSymbol = symbol ","


parens   :: Monad m => PrologParser m a -> PrologParser m a
parens   p = lexeme $ between (symbol "(") (symbol ")") p
brackets :: Monad m => PrologParser m a -> PrologParser m a
brackets p = lexeme $ between (symbol "[") (symbol "]") p
quotes q p = lexeme $ between (char q)     (char q)     p

symbol s = lexeme $ string s
lexeme :: Monad m => PrologParser m a -> PrologParser m a
lexeme p = p <* many layout

layout :: Monad m => PrologParser m ()
layout = space $> ()
     <|> comment $> ()

comment :: Monad m => PrologParser m String
comment = char '%' *> many commentChar <?> "comment"
  where
    commentChar = noneOf "\n"


-- Operator parsers

operation :: Monad m => Integer -> PrologParser m Term
operation maxPrec = toTerm <$> operatorTree maxPrec Empty

  where

    toTerm (Node (Operand t)      _     _    ) = t
    toTerm (Node (Operator a def) ltree rtree) =
      case (ltree, rtree) of
        (Node {}, Node {}) -> toCompound a [ltree, rtree]
        (Node {}, Empty  ) -> toCompound a [ltree]
        (Empty  , Node {}) -> toCompound a [rtree]
        (Empty  , Empty  ) -> Atom a

    toCompound a subtrees = CompoundTerm a (map toTerm subtrees)

operatorTree :: Monad m => Integer -> Tree Operand -> PrologParser m (Tree Operand)
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


operator :: Monad m => Fixity -> Integer -> PrologParser m Operand
operator fix maxPrecedence = operator' <?> describeFixity fix ++ " operator"
  where
    operator' =
      do a <- try nonFunctorAtom
         rslt <- findOperator fix a <$> gets opTable
         case rslt of
           Nothing -> mzero
           Just op ->
             do guard (maxPrecedence >= lbp op)
                return op


-- Syntax instances for term/operator data structures

instance Syntax Term where

  kind _ = "term"

  concrete (Number n)   = show n
  concrete (Atom "[]")  = "[]"
  concrete (Atom a)     = quoteAtom a
  concrete (Variable v) = v

  -- Lists are special-cased to output in syntactic sugar ("[a,b,c|d]") form rather than
  -- explicit form (".(a, .(b, .(c, d)))").'
  concrete (CompoundTerm "." [head, tail]) =
      "[" ++ concrete head ++ concreteList tail ++ "]"
    where
      concreteList (Atom "[]") = ""
      concreteList (CompoundTerm "." [head, tail]) =
        ", " ++ concrete head ++ concreteList tail
      concreteList tail = " | " ++ concrete tail

  concrete (CompoundTerm a subterms) = quoteAtom a ++ "(" ++ concreteSubterms ++ ")"
    where
      concreteSubterms = intercalate ", " (map concrete subterms)


quoteAtom a =
  if needsQuotes a
    then "'" ++ a ++ "'"
    else a

needsQuotes a =
  case parseTest (unquotedAtom <|> symbolicAtom) a of
    Left  _ -> True
    Right _ -> False



instance Syntax Operand where

  kind (Operand  _)     = "operand"
  kind (Operator _ def) = describeFixity (fixity def) ++ " operator"

  concrete (Operand  t)   = concrete t
  concrete (Operator a _) = concrete (Atom a)

describeFixity Infix   = "infix"
describeFixity Prefix  = "prefix"
describeFixity Postfix = "postfix"



instance Syntax HornClause where

  kind (DefiniteClause _ _) = "definite clause"
  kind (GoalClause _)       = "goal clause"

  concrete (DefiniteClause head body) = concrete head ++ " :- " ++ concreteBody
    where
      concreteBody = intercalate ", " (map concrete body)
  concrete (GoalClause goals) = intercalate ", " (map concrete goals)
